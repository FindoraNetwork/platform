sig PublicKey {}
sig PrivateKey {}
sig AssetCode {}
sig Asset { assetType: AssetCode }

sig Txo {
	assets: set Asset
}
fact { all t: Txo | some t.assets }

sig KeyPair { pubkey: disj PublicKey, privkey: disj PrivateKey }

fact { all k : PublicKey | some pair: KeyPair | pair.pubkey = k }
fact { all k : PrivateKey | some pair: KeyPair | pair.privkey = k }

pred isKeyPair[pubk: PublicKey, privk: PrivateKey] {
	one pair: KeyPair | pair.pubkey = pubk and pair.privkey = privk
}

abstract sig Operation {
	prevOp : disj lone Operation,
	signedBy: set PrivateKey
}

fact OnlyOneRoot { lone o: Operation | no o.prevOp }
fact NoCycles    { no o: Operation | some (o.^prevOp & o) }
fact SignEveryOp { no o: Operation | no o.signedBy }

sig Transaction {
  first: Operation,
  last:  Operation
}

fact TxnsAreChains {
  all t: Transaction | t.first in t.last.*prevOp
}

fact TxnsDisjoint {
  all t1: Transaction | all t2: Transaction | (
    t1 != t2 => no (t1.last.*prevOp & t2.last.*prevOp)
  )
}

pred isLoan[t: Transaction, loanType: AssetType, fiatType: AssetType]
{
  loanType != fiatType
  #(t.last.*prevOp) = 2
  one trn1: TransferAsset | one trn2: TransferAsset | (
    trn1 != trn2 and

  )
}


sig DefineAsset extends Operation {
	newCode: disj AssetCode,
	issuer: PublicKey
}

fact NoIrrelevantAssetCodes { all code: AssetCode | one cr: DefineAsset | code = cr.newCode }

sig IssueAsset extends Operation {
	issCode: AssetCode,
	issOutputs: disj set Txo,
	issRecipients : issOutputs -> PublicKey
}
sig TransferAsset extends Operation {
	inputs: set Txo,
	trnOutputs: disj set Txo,
	trnRecipients : trnOutputs -> PublicKey
}

fun outputs(o: Operation): set Txo { o.trnOutputs + o.issOutputs }
fun recipients(o: Operation): Txo -> PublicKey { o.issRecipients + o.trnRecipients }

fact NoReOutput { all o: Operation | no (o.outputs & o.^prevOp.outputs) }
fact oneRecipient {
	all iss: IssueAsset | all txo: iss.issOutputs | one pk: PublicKey | txo -> pk in iss.issRecipients
	all trn: TransferAsset | all txo: trn.trnOutputs | one pk: PublicKey | txo -> pk in trn.trnRecipients
}
pred isLive[a: Asset, o: Operation] {
	some txo: Txo | utxoFor[txo,o] and a in txo.assets
}

//check { all cr: DefineAsset | no cr': DefineAsset | cr' in cr.^prevOp and cr.newCode = cr'.newCode }
fact OnlyIssueCreatedAssets {
	all iss: IssueAsset | some cr: DefineAsset |
	cr in iss.^prevOp and cr.newCode = iss.issCode
}
fact OnlyIssuePermittedAssets {
	all iss: IssueAsset | some cr: DefineAsset | some privk: PrivateKey | (
		isKeyPair[cr.issuer,privk] and	
		cr.newCode = iss.issCode and
		privk in iss.signedBy and
		(all txo: iss.outputs | all a: txo.assets | a.assetType = iss.issCode)
	)
}
fact IssueSomething {
	all iss: IssueAsset | some iss.outputs
}
fact IssueUniqueThings {
	all iss: IssueAsset | all asset: iss.outputs.assets | one o: iss.outputs | asset in o.assets
	all iss: IssueAsset | all asset: iss.outputs.assets | not isLive[asset,iss]
}
fact TransferSomething {
	all iss: TransferAsset | some iss.outputs
}
fact TransferSum {
	all trn: TransferAsset | all asset: trn.trnOutputs.assets | one i: trn.inputs | asset in i.assets
	all trn: TransferAsset | all asset: trn.inputs.assets | one o: trn.trnOutputs | asset in o.assets
}
fact NoReissue {
	all trn: TransferAsset | no (trn.inputs & trn.outputs)
}

pred utxoFor[txos: set Txo, o: Operation] {
	some txos
	all txo: txos | some src : (o.^prevOp.outputs) | txo in src
	no txo: txos | some sink : (o.^prevOp.inputs) | txo in sink
}
pred txoOwnedBy[key: PrivateKey, txo: Txo, o: Operation] {
	utxoFor[txo,o]
	one pubk : PublicKey | isKeyPair[pubk,key] and (
		one o: o.^prevOp | txo in o.outputs and (txo -> pubk) in o.recipients
	)
}
pred assetOwnedBy[key: PrivateKey, a: Asset, o: Operation] {
	one txo: Txo | a in txo.assets and txoOwnedBy[key,txo,o]
}

fact OnlySpendYourOwn {
	all trn: TransferAsset | all txo: trn.inputs | some privk: trn.signedBy | txoOwnedBy[privk,txo,trn]
}

run utxoFor

check CheckNoReissue {
	all iss: IssueAsset | no txo : iss.outputs | utxoFor[txo,iss]
	all iss: IssueAsset | no a : iss.outputs.assets | isLive[a,iss]
} for 10

check TxosUnique {
	no t: Txo | some o1: Operation | some o2: Operation | o1 != o2 and
	t in o1.outputs and t in o2.outputs
} for 8


check TransfersPreserveLiveness {
	all trn: TransferAsset | all o: Operation | trn = o.prevOp => all a: Asset | some trn.prevOp =>
	(isLive[a,trn] => isLive[a,o]) and (isLive[a,o] => isLive[a,trn])
} for 7
check AllLiveAssetsHaveBeenIssuedOnce {
	all o: Operation | all a: Asset | isLive[a,o] =>
	(one iss: IssueAsset | iss in o.^prevOp and a in iss.outputs.assets)
} for 7
check TransfersAreSignedByOwner {
	all o: Operation | all a: Asset | all privk: PrivateKey | (
		(assetOwnedBy[privk,a,o.prevOp] and not (privk in o.prevOp.signedBy)) =>
		assetOwnedBy[privk,a,o]
	)
} for 5


