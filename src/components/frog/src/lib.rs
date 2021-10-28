use {
    ledger::data_model::{TxoSID, Utxo},
    std::collections::BTreeMap,
    zei::xfr::{sig::XfrPublicKey, structs::OwnerMemo},
};

pub type UtxoMap = BTreeMap<XfrPublicKey, BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>>;
