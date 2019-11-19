// Identities are public/private key pairs
sig PublicKey {}
sig PrivateKey {}

// `disj` indicates that no two KeyPairs are allowed to have the same
// pubkey/privkey
sig KeyPair { pubkey: disj PublicKey, privkey: disj PrivateKey }

// Keep alloy from generating public/private key objects not
// associated with KeyPairs
fact { all k : PublicKey  | some pair: KeyPair | pair.pubkey  = k }
fact { all k : PrivateKey | some pair: KeyPair | pair.privkey = k }

pred isKeyPair[pubk: PublicKey, privk: PrivateKey] {
  one pair: KeyPair | pair.pubkey = pubk and pair.privkey = privk
}


// A transaction has pointers to its first and last operations
// (inclusive), and to the previous transaction
sig Transaction {
  first: Operation,
  last:  Operation,
  prevTxn: lone Transaction
}

// All operations in a transaction -- ie, all the things before
// (inclusive) `t.last` which aren't before (exclusive) `t.first`
fun txnOps(t: Transaction): set Operation {t.last.*prevOp - t.first.^prevOp}

fact TxnsAreChains {
  all t: Transaction | t.first in t.last.*prevOp
}

fact TxnsDisjoint {
  all t1: Transaction | all t2: Transaction | (
    t1 != t2 => no (t1.txnOps & t2.txnOps)
  )
}


abstract sig AssetCode {}
sig GenericAsset extends AssetCode {}

sig Asset { assetType: AssetCode }

sig Txo {
  assets: set Asset
}
fact { all t: Txo | some t.assets }


// Operations have a unique (nullable) prev pointer, and a set of
// signatories
abstract sig Operation {
  prevOp : disj lone Operation,
  signedBy: set PrivateKey
}

fact OnlyOneRoot { lone o: Operation | no o.prevOp }
fact NoCycles    { no o: Operation | some (o.^prevOp & o) }
fact SignEveryOp { no o: Operation | no o.signedBy }

fact AllOpsAreInTransactions {
  all o: Operation | some txn: Transaction | o in txn.txnOps
}

// Transactions partition the operation history
fact TxnHistoryMatchesOpHistory {
  // t is first transaction iff t.first is first operation
  all t: Transaction | no t.prevTxn => no t.first.prevOp
  all t: Transaction | no t.first.prevOp => no t.prevTxn

  // t.prevTxn ends immediately before t starts
  all t: Transaction | some t.prevTxn => t.prevTxn.last = t.first.prevOp
}

// Define a new type of asset
sig DefineAsset extends Operation {
  newCode: disj AssetCode,
  issuer: PublicKey // who is allowed to issue it
}

// Prevent alloy from generating asset codes that don't matter
fact NoIrrelevantAssetCodes { all code: AssetCode | one cr: DefineAsset | code = cr.newCode }

// Issue units of a defined asset type
sig IssueAsset extends Operation {
  issCode: AssetCode,
  issOutputs: disj set Txo,
  issRecipients : issOutputs -> PublicKey
}

// Transfer already-existing assets
sig TransferAsset extends Operation {
  inputs: set Txo,
  trnOutputs: disj set Txo,
  trnRecipients : trnOutputs -> PublicKey
}

// generic helpers for common fields of issue/transfer
fun outputs(o: Operation): set Txo { o.trnOutputs + o.issOutputs }
fun recipients(o: Operation): Txo -> PublicKey { o.issRecipients + o.trnRecipients }

// TXOs are never reused
fact NoReOutput { all o: Operation | no (o.outputs & o.^prevOp.outputs) }
// Operations only go to one identity (not strictly necessary)
fact oneRecipient {
  all iss: IssueAsset    | all txo: iss.issOutputs | one pk: PublicKey | txo -> pk in iss.issRecipients
  all trn: TransferAsset | all txo: trn.trnOutputs | one pk: PublicKey | txo -> pk in trn.trnRecipients
}

// are `txos` usable as inputs for `o`?
pred utxoFor[txos: set Txo, o: Operation] {
  some txos
  all txo: txos | some src : (o.^prevOp.outputs) | txo in src
  no  txo: txos | some sink : (o.^prevOp.inputs) | txo in sink
}
// are `txos` usable by `key` as inputs for `o`?
pred txoOwnedBy[key: PrivateKey, txo: Txo, o: Operation] {
  utxoFor[txo,o]
  one pubk : PublicKey | isKeyPair[pubk,key] and (
    one o: o.^prevOp | txo in o.outputs and (txo -> pubk) in o.recipients
  )
}
// is `a` usable by `key` as an input for `o`?
pred assetOwnedBy[key: PrivateKey, a: Asset, o: Operation] {
  one txo: Txo | a in txo.assets and txoOwnedBy[key,txo,o]
}

// Is asset `a` usable by as an input for `o`?
pred isLive[a: Asset, o: Operation] {
  some txo: Txo | utxoFor[txo,o] and a in txo.assets
}

// Asset types must be defined before instances are created
fact OnlyIssueCreatedAssets {
  all iss: IssueAsset | some cr: DefineAsset |
  cr in iss.^prevOp and cr.newCode = iss.issCode
}
// An asset type can only be created with the designated issuer's
// signature and only disbursing to the designated issuer.
fact OnlyIssuePermittedAssets {
  all iss: IssueAsset | some cr: DefineAsset | some privk: PrivateKey | (
    isKeyPair[cr.issuer,privk] and
    cr.newCode = iss.issCode and
    privk in iss.signedBy and
    (all txo: iss.outputs | all a: txo.assets | a.assetType = iss.issCode) and
    (iss.issOutputs -> cr.issuer) = iss.issRecipients
  )
}

// keep alloy from generating empty `IssueAsset` ops
fact IssueSomething {
  all iss: IssueAsset | some iss.outputs
}
// IssueAsset must have some unit of asset in its outputs and that
// unit must not be live in the system beforehand
fact IssueUniqueThings {
  all iss: IssueAsset | all asset: iss.outputs.assets | one o: iss.outputs | asset in o.assets
  all iss: IssueAsset | all asset: iss.outputs.assets | not isLive[asset,iss]
}

// force alloy to make interesting transfers
fact TransferSomething {
  all iss: TransferAsset | some iss.outputs
}
// All input assets must exist in the output, all output assets must
// exist in the input
fact TransferSum {
  all trn: TransferAsset | all asset: trn.trnOutputs.assets | one i: trn.inputs | asset in i.assets
  all trn: TransferAsset | all asset: trn.inputs.assets | one o: trn.trnOutputs | asset in o.assets
}
// You can't have a TXO be both an input & an output
check NoSimpleReissue {
  all trn: TransferAsset | no (trn.inputs & trn.outputs)
} for 7


// You can only transfer assets with the signature of its owner
fact OnlySpendYourOwn {
  all trn: TransferAsset | all txo: trn.inputs | some privk: trn.signedBy | txoOwnedBy[privk,txo,trn]
}


// ------ Defining an "atomic exchange" ------
pred trnIsFrom[ident: KeyPair, trn: TransferAsset, when: Operation] {
    txoOwnedBy[ident.privkey,trn.inputs,when]
}

pred trnIsTo[ident: KeyPair, trn: TransferAsset] {
    trn.trnRecipients = (trn.trnOutputs -> ident.pubkey)
}

// A pair of transfers in a single transaction, going in opposite
// directions
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

// A "loan asset"
sig IOU extends AssetCode {}
fact {
  all kp: KeyPair | lone kp_iou: IOU | some def: DefineAsset | (
    def.newCode = kp_iou and def.issuer = kp.pubkey
  )
}
// A "money asset"
sig Fiat extends AssetCode {}
fact { lone Fiat }

// A loan is an exchange of money for an IOU
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

run utxoFor

// Whenever you have an IssueAsset op, its TXOs and assets are new
check CheckNoReissue {
  all iss: IssueAsset | no txo : iss.outputs | utxoFor[txo,iss]
  all iss: IssueAsset | no a : iss.outputs.assets | isLive[a,iss]
} for 10

// no TXO exists in the output of 2 transactions
check TxosUnique {
  no t: Txo | some o1: Operation | some o2: Operation | o1 != o2 and
  t in o1.outputs and t in o2.outputs
} for 8

// Asset liveness is stable over TransferAsset operations
check TransfersPreserveLiveness {
  all trn: TransferAsset | all o: Operation | trn = o.prevOp => all a: Asset | some trn.prevOp =>
  (isLive[a,trn] => isLive[a,o]) and (isLive[a,o] => isLive[a,trn])
} for 7

// Any live asset can be traced back to a single IssueAsset event
check AllLiveAssetsHaveBeenIssuedOnce {
  all o: Operation | all a: Asset | isLive[a,o] =>
  (one iss: IssueAsset | iss in o.^prevOp and a in iss.outputs.assets)
} for 7

// Asset ownership cannot be changed by any operation not signed by
// the owner
check TransfersAreSignedByOwner {
  all o: Operation | all a: Asset | all privk: PrivateKey | (
    (assetOwnedBy[privk,a,o.prevOp] and not (privk in o.prevOp.signedBy)) =>
    assetOwnedBy[privk,a,o]
  )
} for 6


