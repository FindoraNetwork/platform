sig PublicKey {}
sig PrivateKey {}
abstract sig AssetCode {}
sig GenericAsset extends AssetCode {}
sig IOU extends AssetCode {}
fact {
  all kp: KeyPair | lone kp_iou: IOU | some def: DefineAsset | (
    def.newCode = kp_iou and def.issuer = kp.pubkey
  )
}
sig Fiat extends AssetCode {}
fact { lone Fiat }

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
  last:  Operation,
  prevTxn: lone Transaction
}

fact TxnHistory {
  all t: Transaction | no t.prevTxn => no t.first.prevOp
  all t: Transaction | no t.first.prevOp => no t.prevTxn
  all t: Transaction | some t.prevTxn => t.prevTxn.last = t.first.prevOp
}

fun txnOps(t: Transaction): set Operation {t.last.*prevOp - t.first.^prevOp}

fact TxnsAreChains {
  all t: Transaction | t.first in t.last.*prevOp
}

fact TxnsDisjoint {
  all t1: Transaction | all t2: Transaction | (
    t1 != t2 => no (t1.txnOps & t2.txnOps)
  )
}

fact AllOpsAreInTransactions {
  all o: Operation | some txn: Transaction | o in txn.txnOps
}

pred trnIsFrom[ident: KeyPair, trn: TransferAsset, when: Operation] {
    txoOwnedBy[ident.privkey,trn.inputs,when]
}

pred trnIsTo[ident: KeyPair, trn: TransferAsset] {
    trn.trnRecipients = (trn.trnOutputs -> ident.pubkey)
}

pred atomicExchange[ident1: KeyPair, trn1: TransferAsset,
                    ident2: KeyPair, trn2: TransferAsset,
                    txn: Transaction] {
  #(txn.txnOps) = 2
  ((trn1 + trn2) = txn.txnOps)

  trnIsFrom[ident1,trn1,txn.first.prevOp]
  trnIsTo[ident2,trn1]

  trnIsFrom[ident2,trn2,txn.first.prevOp]
  trnIsTo[ident1,trn2]
}

pred isLoan[lender: KeyPair, borrower: KeyPair, t: Transaction,
            loanType: IOU, fiatType: Fiat,
            theIOU: Asset, theMoney: set Asset]
{
  #(t.txnOps) = 2
  lender != borrower
  one iou: TransferAsset | one loan: TransferAsset | (
    atomicExchange[borrower,iou,lender,loan,t] and

    iou.inputs.assets  = theIOU and
    loan.inputs.assets = theMoney and

    (iou.inputs.assets.assetType = loanType) and
    (loan.inputs.assets.assetType = fiatType)
  )
}

run atomicExchange for 7
run isLoan for 7

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
		(all txo: iss.outputs | all a: txo.assets | a.assetType = iss.issCode) and
               (iss.issOutputs -> cr.issuer) = iss.issRecipients
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


