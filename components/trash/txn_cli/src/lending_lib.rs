use crate::data_lib::*;
use crate::txn_lib::*;
use credentials::{
    credential_commit, credential_sign, credential_user_key_gen,
    Credential as WrapperCredential,
};
use crypto::basics::hybrid_encryption::hybrid_encrypt_with_x25519_key;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AssetRules, AssetTypeCode, TransferType, TxoRef};
use ledger::policies::{DebtMemo, Fraction};
use ledger::ser_fail;
use ledger_api_service::RestfulLedgerAccess;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use submission_api::RestfulLedgerUpdate;
use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
use zei::api::anon_creds::ac_confidential_open_commitment;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::asset_record::{open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{AssetRecordTemplate, BlindAssetRecord, OwnerMemo, TracerMemo};

/// Merges two asset records.
/// # Arguments
/// * `key_pair`: key pair of the two records.
/// * `no_replay_token`:
/// * `sid1`: SID of the first record.
/// * `sid2`: SID of the second record.
/// * `blind_asset_record1`: blind asset record of the first record.
/// * `blind_asset_record2`: blind asset record of the second record.
/// * `token_code`: asset token code of the two records.
pub(crate) fn merge_records(
    key_pair: &XfrKeyPair,
    seq_id: u64,
    sid1: TxoRef,
    sid2: TxoRef,
    blind_asset_record1: (BlindAssetRecord, Option<OwnerMemo>),
    blind_asset_record2: (BlindAssetRecord, Option<OwnerMemo>),
    token_code: AssetTypeCode,
) -> Result<TransactionBuilder> {
    let oar1 = open_blind_asset_record(
        &blind_asset_record1.0,
        &blind_asset_record1.1,
        &key_pair,
    )
    .c(d!(PlatformError::ZeiError(None)))?;
    let oar2 = open_blind_asset_record(
        &blind_asset_record2.0,
        &blind_asset_record2.1,
        &key_pair,
    )
    .c(d!(PlatformError::ZeiError(None)))?;
    if oar1.get_record_type() != oar2.get_record_type() {
        return Err(eg!(PlatformError::InputsError(None)));
    }
    let amount1 = *oar1.get_amount();
    let amount2 = *oar2.get_amount();

    // Transfer Operation
    let template = AssetRecordTemplate::with_no_asset_tracing(
        amount1 + amount2,
        token_code.val,
        oar1.get_record_type(),
        key_pair.get_pk(),
    );
    let xfr_op = TransferOperationBuilder::new()
        .add_input(sid1, oar1, None, None, amount1)
        .c(d!())?
        .add_input(sid2, oar2, None, None, amount2)
        .c(d!())?
        .add_output(&template, None, None, None)
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(key_pair)
        .c(d!())?
        .transaction()
        .c(d!())?;

    // Merge records
    let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
    txn_builder.add_operation(xfr_op).transaction();
    Ok(txn_builder)
}

/// Loads funds.
/// # Arguments
/// * `issuer_id`: issuer ID.
/// * `recipient_id`: recipient's ID.
/// * `amount`: amount to load.
/// * `memo_file`: path to store the tracer and owner memos, optional.
/// * `rest_client`: http client
pub fn load_funds<T>(
    data_dir: &str,
    issuer_id: u64,
    recipient_id: u64,
    amount: u64,
    rest_client: &mut T,
) -> Result<()>
where
    T: RestfulLedgerAccess + RestfulLedgerUpdate,
{
    // Get data
    let data = load_data(data_dir).c(d!())?;
    let issuer_key_pair = &data.get_asset_issuer_key_pair(issuer_id).c(d!())?;
    let recipient = &data.borrowers[recipient_id as usize];
    let recipient_key_pair =
        &data.clone().get_borrower_key_pair(recipient_id).c(d!())?;

    // Get or define fiat asset
    let token_code = if let Some(code) = &data.fiat_code {
        AssetTypeCode::new_from_base64(code).c(d!())?
    } else {
        let fiat_code = AssetTypeCode::gen_random();
        let txn_builder = define_asset(
            data_dir,
            rest_client.get_block_commit_count().c(d!())?,
            true,
            issuer_key_pair,
            fiat_code,
            "Fiat asset",
            AssetRules::default(),
            None,
        )
        .c(d!())?;
        // Store data before submitting the transaction to avoid data overwriting
        let data = load_data(data_dir).c(d!())?;
        rest_client
            .submit_transaction(&txn_builder.transaction())
            .c(d!())?;
        store_data_to_file(data, data_dir).c(d!())?;
        fiat_code
    };

    // Issue and transfer asset
    let seq_id = rest_client.get_block_commit_count().c(d!())?;
    let txn_builder = issue_and_transfer_asset(
        data_dir,
        seq_id,
        issuer_key_pair,
        recipient_key_pair,
        amount,
        token_code,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        None,
        None,
        None,
        None,
        None,
    )
    .c(d!())?;

    // Submit transaction and get the new record
    let sid_new = submit_and_get_sids(rest_client, txn_builder).c(d!())?[0];
    let blind_asset_record_new = (rest_client.get_utxo(sid_new).unwrap().utxo.0).record;
    // Merge records
    let sid_merged = if let Some(sid_pre) = recipient.fiat_utxo {
        let blind_asset_record_pre =
            (rest_client.get_utxo(sid_pre).unwrap().utxo.0).record;
        let txn_builder = merge_records(
            recipient_key_pair,
            rest_client.get_block_commit_count().c(d!())?,
            TxoRef::Absolute(sid_pre),
            TxoRef::Absolute(sid_new),
            (blind_asset_record_pre, None), // no associated owner memo with blind asset record
            (blind_asset_record_new, None), // no associated owner memo with blind asset record
            token_code,
        )
        .c(d!())?;

        submit_and_get_sids(rest_client, txn_builder).c(d!())?[0]
    } else {
        sid_new
    };

    // Update data
    let mut data = load_data(data_dir).c(d!())?;
    data.borrowers[recipient_id as usize].balance = recipient.balance + amount;
    data.borrowers[recipient_id as usize].fiat_utxo = Some(sid_merged);
    store_data_to_file(data, data_dir)
}

/// Fulfills a loan.
/// # Arguments
/// * `loan_id`: loan ID.
/// * `issuer_id`: issuer ID.
/// * `rest_client`: path to store the asset tracer memo and owner memo, optional.
pub fn fulfill_loan<T>(
    data_dir: &str,
    loan_id: u64,
    issuer_id: u64,
    memo_file: Option<&str>,
    rest_client: &mut T,
) -> Result<()>
where
    T: RestfulLedgerUpdate + RestfulLedgerAccess,
{
    // Get data
    let mut data = load_data(data_dir).c(d!())?;
    let issuer_key_pair = &data.get_asset_issuer_key_pair(issuer_id).c(d!())?;
    let loan = &data.loans[loan_id as usize].clone();

    // Check if loan has been fulfilled
    match loan.status {
        LoanStatus::Declined => {
            return Err(eg!(PlatformError::InputsError(Some(format!(
                "Loan {} has been declined.",
                loan_id
            )))));
        }
        LoanStatus::Active => {
            return Err(eg!(PlatformError::InputsError(Some(format!(
                "Loan {} has been fulfilled.",
                loan_id
            )))));
        }
        LoanStatus::Complete => {
            return Err(eg!(PlatformError::InputsError(Some(format!(
                "Loan {} has been paid off.",
                loan_id
            )))));
        }
        _ => {}
    }

    let tracer_enc_keys = data.get_asset_tracer_key_pair(issuer_id).c(d!())?.enc_key;
    let lender_id = loan.lender;
    let lender = &data.lenders[lender_id as usize];
    let lender_key_pair = &data.get_lender_key_pair(loan.lender).c(d!())?;
    let borrower_id = loan.borrower;
    let borrower = &data.borrowers[borrower_id as usize].clone();
    let borrower_key_pair = &data.get_borrower_key_pair(borrower_id).c(d!())?;
    let amount = loan.amount;

    // Credential check
    let credential_id = if let Some(id) = borrower.credentials {
        id as usize
    } else {
        println!("Credential is required. Use create_or_overwrite_credential.");
        return Err(eg!(PlatformError::InputsError(None)));
    };
    let credential = &data.credentials[credential_id as usize];
    let credential_issuer_id = credential.credential_issuer;

    // Check if the credential values meet the requirements
    let values = credential.values.clone();
    let mut value_iter = values.iter();
    let requirements = lender.requirements.clone();
    let mut requirement_iter = requirements.iter();
    let mut count = 0;
    let mut attributes = Vec::new();
    let mut attribute_names = Vec::new();
    let mut attibutes_with_value_as_vec = Vec::new();
    let mut reveal_map = Vec::new();

    // For each credential attribute:
    // If the lender doesn't have a requirement, skip it
    // Otherwise:
    // * If the borrower doesn't provide the corresponding attribute value, return an error
    // * Otherwise, check if the value meets the requirement
    while count < 3 {
        if let Some(requirement_next) = requirement_iter.next() {
            if let Some(requirement) = requirement_next {
                if let Some(value_next) = value_iter.next() {
                    if let Some(value) = value_next {
                        let requirement_u64 = parse_to_u64(requirement).c(d!())?;
                        let requirement_type =
                            CredentialIndex::get_requirement_type(count);
                        match requirement_type {
                            ComparisonType::AtLeast => {
                                if parse_to_u64(value).c(d!())? < requirement_u64 {
                                    // Update loans data
                                    data.loans[loan_id as usize].status =
                                        LoanStatus::Declined;
                                    store_data_to_file(data, data_dir).c(d!())?;
                                    return Err(eg!(PlatformError::InputsError(Some(
                                        format!(
                                            "Credential value should be at least: {}.",
                                            requirement_u64
                                        )
                                    ))));
                                }
                            }
                            _ => {
                                if parse_to_u64(value).c(d!())? != requirement_u64 {
                                    // Update loans data
                                    data.loans[loan_id as usize].status =
                                        LoanStatus::Declined;
                                    store_data_to_file(data, data_dir).c(d!())?;
                                    return Err(eg!(PlatformError::InputsError(Some(
                                        format!(
                                            "Credential value should be at least: {}.",
                                            requirement_u64
                                        )
                                    ))));
                                }
                            }
                        }
                        let attribute =
                            CredentialIndex::get_credential_index(count).c(d!())?;
                        let value_bytes = value.as_bytes();
                        attributes.push((attribute.get_name().to_string(), value_bytes));
                        attribute_names.push(attribute.get_name().to_string());
                        attibutes_with_value_as_vec.push((
                            attribute.get_name().to_string(),
                            value_bytes.to_vec(),
                        ));
                        reveal_map.push(true);
                    } else {
                        println!(
                            "Missing credential value. Use subcommand borrower create_or_overwrite_credential."
                        );
                        return Err(eg!(PlatformError::InputsError(None)));
                    }
                } else {
                    println!("More credential value expected.");
                    return Err(eg!(PlatformError::InputsError(None)));
                }
            }
        } else {
            println!("More credential requirement expected.");
            return Err(eg!(PlatformError::InputsError(None)));
        }
        count += 1;
    }

    // Prove and attest the credential
    let (credential_issuer_public_key, credential_issuer_secret_key) = data
        .get_credential_issuer_key_pair(credential_issuer_id)
        .c(d!())?;
    let mut prng: ChaChaRng = ChaChaRng::from_entropy();
    let (user_pk, user_secret_key) =
        credential_user_key_gen(&mut prng, &credential_issuer_public_key);
    let signature = credential_sign(
        &mut prng,
        &credential_issuer_secret_key,
        &user_pk,
        &attributes,
    )
    .unwrap();
    let wrapper_credential = WrapperCredential {
        attributes: attibutes_with_value_as_vec,
        issuer_pub_key: credential_issuer_public_key,
        signature,
    };
    let ac_credential = wrapper_credential
        .to_ac_credential()
        .c(d!(PlatformError::ZeiError(None)))?;
    let (_, _, commitment_key) =
        credential_commit(&mut prng, &user_secret_key, &wrapper_credential, b"")
            .unwrap();

    // Store the tracer memo to file
    if let Some(file) = memo_file {
        let ciphertext = ac_confidential_open_commitment(
            &mut prng,
            &user_secret_key.get_ref(),
            &ac_credential,
            &commitment_key,
            &tracer_enc_keys.attrs_enc_key,
            &reveal_map,
            &[],
        )
        .c(d!(PlatformError::ZeiError(None)))?
        .ctexts;
        let li_pk = tracer_enc_keys.lock_info_enc_key.clone();
        let tracer_memo = TracerMemo {
            enc_key: tracer_enc_keys,
            lock_amount: None,
            lock_asset_type: None,
            lock_attributes: ciphertext,
            lock_info: hybrid_encrypt_with_x25519_key(&mut prng, &li_pk, &[]),
        };
        store_tracer_memo_to_file(file, tracer_memo).c(d!())?;
    }

    // Get or define fiat asset
    let fiat_code = if let Some(code) = data.fiat_code.clone() {
        println!("Fiat code: {}", code);
        AssetTypeCode::new_from_base64(&code).c(d!())?
    } else {
        let fiat_code = AssetTypeCode::gen_random();
        let seq_id = rest_client.get_block_commit_count().c(d!())?;
        let txn_builder = define_asset(
            data_dir,
            seq_id,
            true,
            issuer_key_pair,
            fiat_code,
            "Fiat asset",
            AssetRules::default(),
            None,
        )
        .c(d!())?;
        // Store data before submitting the transaction to avoid data overwriting
        let data = load_data(data_dir).c(d!())?;
        rest_client
            .submit_transaction(&txn_builder.transaction())
            .c(d!())?;
        store_data_to_file(data, data_dir).c(d!())?;
        fiat_code
    };

    // Issue and transfer fiat token
    let fiat_txn_file = &format!("{}/{}", data_dir, "fiat_txn_file");
    let seq_id = rest_client.get_block_commit_count().c(d!())?;
    let txn_builder = issue_and_transfer_asset(
        data_dir,
        seq_id,
        issuer_key_pair,
        lender_key_pair,
        amount,
        fiat_code,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        None,
        Some(fiat_txn_file),
        None,
        None,
        None,
    )
    .c(d!())?;
    let fiat_sid = submit_and_get_sids(rest_client, txn_builder).c(d!())?[0];
    println!("Fiat sid: {}", fiat_sid.0);
    let (_, owner_memo) =
        load_blind_asset_record_and_owner_memo_from_file(fiat_txn_file).c(d!())?;
    let fiat_open_asset_record =
        query_open_asset_record(rest_client, fiat_sid, lender_key_pair, &owner_memo)
            .c(d!())?;

    // Define debt asset
    let debt_code = AssetTypeCode::gen_random();
    println!(
        "Generated debt code: {}",
        serde_json::to_string(&debt_code.val).c(d!(ser_fail!()))?
    );
    let memo = DebtMemo {
        interest_rate: Fraction::new(loan.interest_per_mille, 1000),
        fiat_code,
        loan_amount: amount,
    };
    let memo_str = serde_json::to_string(&memo).c(d!(ser_fail!()))?;
    let txn_builder = define_asset(
        data_dir,
        rest_client.get_block_commit_count().c(d!())?,
        false,
        borrower_key_pair,
        debt_code,
        &memo_str,
        AssetRules::default(),
        None,
    )
    .c(d!())?;
    // Store data before submitting the transaction to avoid data overwriting
    rest_client
        .submit_transaction(&txn_builder.transaction())
        .c(d!())?;
    store_data_to_file(data, data_dir).c(d!())?;

    // Issue and transfer debt token
    let debt_txn_file = "debt_txn_file";
    let seq_id = rest_client.get_block_commit_count().c(d!())?;
    let txn_builder = issue_and_transfer_asset(
        data_dir,
        seq_id,
        borrower_key_pair,
        borrower_key_pair,
        amount,
        debt_code,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        None,
        Some(debt_txn_file),
        None,
        None,
        None,
    )
    .c(d!())?;
    let debt_sid = submit_and_get_sids(rest_client, txn_builder).c(d!())?[0];
    println!("Debt sid: {}", debt_sid.0);
    let debt_open_asset_record =
        load_open_asset_record_from_file(debt_txn_file, borrower_key_pair).c(d!())?;

    // Initiate loan
    let lender_template = AssetRecordTemplate::with_no_asset_tracing(
        amount,
        debt_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        lender_key_pair.get_pk(),
    );
    let borrower_template = AssetRecordTemplate::with_no_asset_tracing(
        amount,
        fiat_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        borrower_key_pair.get_pk(),
    );
    let xfr_op = TransferOperationBuilder::new()
        .add_input(
            TxoRef::Absolute(fiat_sid),
            fiat_open_asset_record,
            None,
            None,
            amount,
        )
        .c(d!())?
        .add_input(
            TxoRef::Absolute(debt_sid),
            debt_open_asset_record,
            None,
            None,
            amount,
        )
        .c(d!())?
        .add_output(&lender_template, None, None, None)
        .c(d!())?
        .add_output(&borrower_template, None, None, None)
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(lender_key_pair)
        .c(d!())?
        .sign(borrower_key_pair)
        .c(d!())?
        .transaction()
        .c(d!())?;

    let mut txn_builder =
        TransactionBuilder::from_seq_id(rest_client.get_block_commit_count().c(d!())?);
    txn_builder.add_operation(xfr_op);

    // Submit transaction
    let sids_new = submit_and_get_sids(rest_client, txn_builder).c(d!())?;

    // Merge records
    let fiat_sid_merged = if let Some(sid_pre) = borrower.fiat_utxo {
        let blind_asset_record_pre =
            (rest_client.get_utxo(sid_pre).c(d!())?.utxo.0).record;
        let blind_asset_record_new =
            (rest_client.get_utxo(sids_new[1]).c(d!())?.utxo.0).record;
        let txn_builder = merge_records(
            borrower_key_pair,
            rest_client.get_block_commit_count().c(d!())?,
            TxoRef::Absolute(sid_pre),
            TxoRef::Absolute(sids_new[1]),
            (blind_asset_record_pre, None),
            (blind_asset_record_new, None),
            fiat_code,
        )
        .c(d!())?;
        submit_and_get_sids(rest_client, txn_builder).c(d!())?[0]
    } else {
        sids_new[1]
    };
    println!(
        "New debt utxo sid: {}, fiat utxo sid: {}.",
        sids_new[0].0, fiat_sid_merged.0
    );

    // Update data
    let mut data = load_data(data_dir).c(d!())?;
    data.loans[loan_id as usize].issuer = Some(issuer_id);
    data.fiat_code = Some(fiat_code.to_base64());
    data.loans[loan_id as usize].status = LoanStatus::Active;
    data.loans[loan_id as usize].code = Some(debt_code.to_base64());
    data.loans[loan_id as usize].debt_utxo = Some(sids_new[0]);
    data.borrowers[borrower_id as usize].balance = borrower.balance + amount;
    data.borrowers[borrower_id as usize].fiat_utxo = Some(fiat_sid_merged);
    store_data_to_file(data, data_dir)
}

/// Pays loan.
/// # Arguments
/// * `loan_id`: loan ID.
/// * `amount`: amount to pay.
/// * `rest_client`: http client
pub fn pay_loan<T>(
    data_dir: &str,
    loan_id: u64,
    amount: u64,
    rest_client: &mut T,
) -> Result<()>
where
    T: RestfulLedgerAccess + RestfulLedgerUpdate,
{
    // Get data
    let data = load_data(data_dir).c(d!())?;
    let loan = &data.loans[loan_id as usize];

    // Check if it's valid to pay
    match loan.status {
        LoanStatus::Requested => {
            return Err(eg!(PlatformError::InputsError(Some(format!(
                "Loan {} hasn't been fulfilled yet. Use issuer fulfill_loan.",
                loan_id
            )))));
        }
        LoanStatus::Declined => {
            return Err(eg!(PlatformError::InputsError(Some(format!(
                "Loan {} has been declined.",
                loan_id
            )))));
        }
        LoanStatus::Complete => {
            return Err(eg!(PlatformError::InputsError(Some(format!(
                "Loan {} has been paid off.",
                loan_id
            )))));
        }
        _ => {}
    }

    let lender_id = loan.lender;
    let borrower_id = loan.borrower;
    let borrower = &data.borrowers[borrower_id as usize];
    let lender_key_pair = &data.get_lender_key_pair(lender_id).c(d!())?;
    let borrower_key_pair = &data.get_borrower_key_pair(borrower_id).c(d!())?;

    // Check if funds are sufficient
    if amount > borrower.balance {
        println!("Insufficient funds. Use --load_funds to load more funds.");
        return Err(eg!(PlatformError::InputsError(None)));
    }

    // Check if the amount meets the minimum requirement, i.e., the fee
    let fee = ledger::policies::calculate_fee(
        loan.balance,
        Fraction::new(loan.interest_per_mille, 1000),
    );
    if amount < fee {
        println!("Payment amount should be at least: {}", fee);
        return Err(eg!(PlatformError::InputsError(None)));
    }

    // Get the amount to burn the balance, and the total amount the borrow will spend
    let mut amount_to_burn = amount - fee;
    if amount_to_burn > loan.balance {
        println!("Paying {} is enough.", loan.balance);
        amount_to_burn = loan.balance;
    }
    let amount_to_spend = amount_to_burn + fee;
    println!(
        "The borrower will spend {} to burn {}.",
        amount_to_spend, amount_to_burn
    );

    // Get fiat and debt sids
    let fiat_sid = if let Some(sid) = borrower.fiat_utxo {
        sid
    } else {
        println!("Missing fiat utxo in the borrower record. Try --fulfill_loan.");
        return Err(eg!(PlatformError::InputsError(None)));
    };
    let debt_sid = if let Some(sid) = loan.debt_utxo {
        sid
    } else {
        println!("Missing debt utxo in the loan record. Try --fulfill_loan.");
        return Err(eg!(PlatformError::InputsError(None)));
    };

    // Get fiat and debt open asset records
    let fiat_open_asset_record =
        query_open_asset_record(rest_client, fiat_sid, lender_key_pair, &None)
            .c(d!())?;
    let debt_open_asset_record =
        query_open_asset_record(rest_client, debt_sid, borrower_key_pair, &None)
            .c(d!())?;

    // Get fiat and debt codes
    let fiat_code = if let Some(code) = data.clone().fiat_code {
        AssetTypeCode::new_from_base64(&code).c(d!())?
    } else {
        println!("Missing fiat code. Try --active_loan.");
        return Err(eg!(PlatformError::InputsError(None)));
    };
    let debt_code = if let Some(code) = &loan.code {
        AssetTypeCode::new_from_base64(&code).c(d!())?
    } else {
        println!("Missing debt code in the loan record. Try --fulfill_loan.");
        return Err(eg!(PlatformError::InputsError(None)));
    };

    println!(
        "Fiat code: {}",
        serde_json::to_string(&fiat_code.val).c(d!())?
    );
    println!(
        "Debt code: {}",
        serde_json::to_string(&debt_code.val).c(d!())?
    );

    // Get templates
    let spend_template = AssetRecordTemplate::with_no_asset_tracing(
        amount_to_spend,
        fiat_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        lender_key_pair.get_pk(),
    );
    let burn_template = AssetRecordTemplate::with_no_asset_tracing(
        amount_to_burn,
        debt_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        XfrPublicKey::zei_from_bytes(&[0; 32]).c(d!(PlatformError::ZeiError(None)))?,
    );
    let lender_template = AssetRecordTemplate::with_no_asset_tracing(
        loan.balance - amount_to_burn,
        debt_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        lender_key_pair.get_pk(),
    );
    let borrower_template = AssetRecordTemplate::with_no_asset_tracing(
        borrower.balance - amount_to_spend,
        fiat_code.val,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        borrower_key_pair.get_pk(),
    );
    let op = TransferOperationBuilder::new()
        .add_input(
            TxoRef::Absolute(debt_sid),
            debt_open_asset_record,
            None,
            None,
            amount_to_burn,
        )
        .c(d!())?
        .add_input(
            TxoRef::Absolute(fiat_sid),
            fiat_open_asset_record,
            None,
            None,
            amount_to_spend,
        )
        .c(d!())?
        .add_output(&spend_template, None, None, None)
        .c(d!())?
        .add_output(&burn_template, None, None, None)
        .c(d!())?
        .add_output(&lender_template, None, None, None)
        .c(d!())?
        .add_output(&borrower_template, None, None, None)
        .c(d!())?
        .create(TransferType::DebtSwap)
        .c(d!())?
        .sign(borrower_key_pair)
        .c(d!())?
        .transaction()
        .c(d!())?;

    let mut txn_builder =
        TransactionBuilder::from_seq_id(rest_client.get_block_commit_count().c(d!())?);
    txn_builder.add_operation(op).transaction();

    // Submit transaction and update data
    let sids = submit_and_get_sids(rest_client, txn_builder).c(d!())?;

    let mut data = load_data(data_dir).c(d!())?;
    let balance = loan.balance - amount_to_burn;
    if balance == 0 {
        data.loans[loan_id as usize].status = LoanStatus::Complete;
    }
    data.loans[loan_id as usize].balance = balance;
    data.loans[loan_id as usize].payments = loan.payments + 1;
    data.loans[loan_id as usize].debt_utxo = Some(sids[2]);
    data.borrowers[borrower_id as usize].balance = borrower.balance - amount_to_spend;
    data.borrowers[borrower_id as usize].fiat_utxo = Some(sids[3]);

    store_data_to_file(data, data_dir)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ledger::data_model::TxoSID;
    use network::MockLedgerStandalone;
    use tempfile::tempdir;
    #[test]
    fn test_merge_records() {
        // Create key pair
        let mut prng: ChaChaRng = ChaChaRng::from_entropy();
        let key_pair = XfrKeyPair::generate(&mut prng);

        // Build blind asset records
        let code = AssetTypeCode::gen_random();
        let asset_record_type =
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
        let (bar1, _, memo1) = get_blind_asset_record_and_memos(
            key_pair.get_pk(),
            1000,
            code,
            asset_record_type,
            None,
        )
        .unwrap();
        let (bar2, _, memo2) = get_blind_asset_record_and_memos(
            key_pair.get_pk(),
            500,
            code,
            asset_record_type,
            None,
        )
        .unwrap();

        // Merge records
        assert!(
            merge_records(
                &key_pair,
                0, // OK to use default as this doesn't get submitted
                TxoRef::Absolute(TxoSID(1)),
                TxoRef::Absolute(TxoSID(2)),
                (bar1, memo1),
                (bar2, memo2),
                code
            )
            .is_ok()
        );
    }

    // TODO: update `init_data.json`, and uncomment this case
    //
    // #[test]
    // fn test_request_fulfill_and_pay_loan() {
    //     let mut ledger_standalone = MockLedgerStandalone::new_mock(1);

    //     // Load funds
    //     let tmp_dir = tempdir().unwrap();
    //     let data_dir = tmp_dir.path().to_str().unwrap();

    //     let funds_amount = 1000;
    //     load_funds(data_dir, 0, 0, funds_amount, &mut ledger_standalone).unwrap();
    //     let data = load_data(data_dir).unwrap();

    //     assert_eq!(data.borrowers[0].balance, funds_amount);

    //     tmp_dir.close().unwrap();

    //     // Request a loan
    //     let tmp_dir = tempdir().unwrap();
    //     let data_dir = tmp_dir.path().to_str().unwrap();

    //     let loan_amount = 1200;
    //     let mut data = load_data(data_dir).unwrap();
    //     data.add_loan(data_dir, 0, 0, loan_amount, 100, 8).unwrap();

    //     assert_eq!(data.loans.len(), 1);

    //     // Fulfill the loan request
    //     fulfill_loan(data_dir, 0, 0, None, &mut ledger_standalone).unwrap();
    //     data = load_data(data_dir).unwrap();

    //     assert_eq!(data.loans[0].status, LoanStatus::Active);
    //     assert_eq!(data.loans[0].balance, loan_amount);

    //     // Pay loan
    //     let payment_amount = 200;
    //     pay_loan(data_dir, 0, payment_amount, &mut ledger_standalone).unwrap();
    //     data = load_data(data_dir).unwrap();

    //     assert_eq!(data.loans[0].payments, 1);

    //     tmp_dir.close().unwrap();
    // }
}
