use crate::data_lib::*;
use crate::lending_lib::{fulfill_loan, load_funds, pay_loan};
use crate::txn_lib::{
    define_asset, issue_and_transfer_asset, query_open_asset_record, submit_and_get_sids,
};
use clap::{App, Arg, SubCommand};
use credentials::u8_slice_to_u32_vec;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{
    AccountAddress, AssetRules, AssetTypeCode, KVHash, SignatureRules, TxoRef, TxoSID,
};
use ledger::{error_location, ser_fail};
use ledger_api_service::RestfulLedgerAccess;
use log::debug;
use query_api::RestfulQueryServerAccess;
use sparse_merkle_tree::Key;
use std::env;
use submission_api::RestfulLedgerUpdate;
use txn_builder::{BuildsTransactions, TransactionBuilder};
use zei::setup::PublicParams;
use zei::xfr::asset_record::AssetRecordType;
use zei::xfr::structs::AssetTracingPolicy;

fn inputs_error(msg: &str) -> Result<(), PlatformError> {
    Err(PlatformError::InputsError(msg.to_owned()))
}

fn io_error(msg: &str) -> Result<(), PlatformError> {
    Err(PlatformError::IoError(msg.to_owned()))
}

/// Processes the `asset_issuer` subcommand.
///
/// Subcommands under `asset_issuer`
/// * `sign_up`
/// * `store_sids`
/// * `define_asset`
/// * `issue_asset`
/// * `transfer_asset`
/// * `issue_and_transfer_asset`
///
/// # Arguments
/// * `asset_issuer_matches`: subcommands and arguments under the `asset_issuer` subcommand.
/// * `txn_file`: path to store the transaction file.
pub(crate) fn process_asset_issuer_cmd<T: RestfulLedgerAccess + RestfulLedgerUpdate>(
    asset_issuer_matches: &clap::ArgMatches,
    data_dir: &str,
    txn_file: Option<&str>,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    match asset_issuer_matches.subcommand() {
        ("sign_up", Some(sign_up_matches)) => {
            let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
                name_arg.to_owned()
            } else {
                eprintln!(
                    "Name is required to sign up an asset issuer account. Use --name."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let mut data = load_data(data_dir)?;
            data.add_asset_issuer(data_dir, name)
        }
        ("store_sids", Some(store_sids_matches)) => {
            let file = if let Some(file_arg) = store_sids_matches.value_of("file") {
                file_arg
            } else {
                eprintln!("Path is required to store the sids. Use --path.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let sids = if let Some(indices_arg) = store_sids_matches.value_of("indices")
            {
                indices_arg
            } else {
                eprintln!("Indices are required to store the sids. Use --indices.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            store_sids_to_file(file, sids)
        }
        ("store_memos", Some(store_bar_and_memos_matches)) => {
            debug!("store_memos: entering");
            let data = load_data(data_dir)?;
            let (issuer_pub_key, policy) = if let Some(id_arg) =
                asset_issuer_matches.value_of("id")
            {
                let issuer_id = parse_to_u64(id_arg)?;
                debug!("store_memos: got issuer_id={}", issuer_id);
                let issuer_pub_key = data.get_asset_issuer_key_pair(issuer_id)?.get_pk();
                debug!("store_memos: got issuer_pub_key={:?}", &issuer_pub_key);
                let tracer_enc_keys = data.get_asset_tracer_key_pair(issuer_id)?.enc_key;
                debug!("store_memos: got tracer_enc_keys={:?}", &tracer_enc_keys);
                let policy = AssetTracingPolicy {
                    enc_keys: tracer_enc_keys,
                    asset_tracking: true,
                    identity_tracking: None,
                };
                (issuer_pub_key, policy)
            } else {
                eprintln!(
                    "Asset issuer id is required to store the tracer and owner memos. Use asset_issuer --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let amount = if let Some(amount_arg) =
                store_bar_and_memos_matches.value_of("amount")
            {
                parse_to_u64(amount_arg)?
            } else {
                eprintln!(
                    "Asset amount is required to store the tracer and owner memos. Use --amount."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let confidential_amount =
                store_bar_and_memos_matches.is_present("confidential_amount");
            let record_type = AssetRecordType::from_booleans(confidential_amount, false);
            debug!("store_memos: about to get token code");
            let token_code = if let Some(token_code) =
                store_bar_and_memos_matches.value_of("token_code")
            {
                AssetTypeCode::new_from_base64(token_code)?
            } else {
                eprintln!(
                    "Asset token code is required to store the tracer and owner memos. Use --token_code."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let file = if let Some(file_arg) =
                store_bar_and_memos_matches.value_of("file")
            {
                file_arg
            } else {
                eprintln!(
                    "Path is required to store the tracer and owner memos. Use --file."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            debug!("store_memos: about to call get_and_store_memos_to_file");
            get_and_store_memos_to_file(
                file,
                issuer_pub_key,
                amount,
                token_code,
                record_type,
                Some(policy),
            )
        }
        ("define_asset", Some(define_asset_matches)) => {
            let fiat_asset = define_asset_matches.is_present("fiat");
            let data = load_data(data_dir)?;
            let issuer_key_pair = if let Some(id_arg) =
                asset_issuer_matches.value_of("id")
            {
                let issuer_id = parse_to_u64(id_arg)?;
                data.get_asset_issuer_key_pair(issuer_id)?
            } else {
                eprintln!(
                    "Asset issuer id is required to define an asset. Use asset_issuer --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let token_code = define_asset_matches.value_of("token_code");
            let memo = if let Some(memo) = define_asset_matches.value_of("memo") {
                memo
            } else {
                "{}"
            };

            // Define asset rules
            let mut asset_rules = AssetRules::default();

            if define_asset_matches.is_present("non_transferable") {
                asset_rules.set_transferable(false);
            }

            if define_asset_matches.is_present("updatable") {
                asset_rules.set_transferable(true);
            }

            if let Some(units) = define_asset_matches.value_of("max_units") {
                let max_units = parse_to_u64(units)?;
                asset_rules.set_max_units(Some(max_units));
            }

            if let Some(co_signer_ids) = define_asset_matches.value_of("cosigners") {
                let recipient_ids = parse_to_u64_vec(co_signer_ids)?;
                let mut cosigners = vec![];
                let weights;
                let threshold;
                for id in recipient_ids {
                    // TODO (redmine issue #35) should be a generic key not a borrower
                    let co_signer_key = data.get_borrower_key_pair(id)?.get_pk();
                    cosigners.push(co_signer_key);
                }

                let num_cosigners = cosigners.len();

                if let Some(cosignature_weights) =
                    define_asset_matches.value_of("cosignature_weights")
                {
                    weights = parse_to_u64_vec(cosignature_weights)?;
                    if weights.len() != num_cosigners {
                        return Err(PlatformError::InputsError(error_location!()));
                    }
                } else {
                    weights = vec![1; num_cosigners];
                }

                if let Some(thresh) = define_asset_matches.value_of("threshold") {
                    threshold = parse_to_u64(thresh)?;
                } else {
                    threshold = num_cosigners as u64;
                }

                let signature_rules = Some(SignatureRules {
                    threshold,
                    weights: cosigners
                        .iter()
                        .cloned()
                        .zip(weights.iter().cloned())
                        .collect(),
                });

                asset_rules.set_transfer_multisig_rules(signature_rules);
            }

            // Get token code
            let asset_token: AssetTypeCode;
            if let Some(token_code) = token_code {
                asset_token = AssetTypeCode::new_from_base64(token_code)?;
                println!(
                    "Generating asset with token code {:?}: {:?} from {}",
                    asset_token.to_base64(),
                    asset_token.val,
                    token_code
                );
            } else {
                asset_token = AssetTypeCode::gen_random();
                println!(
                    "Creating asset with token code {:?}: {:?}",
                    asset_token.to_base64(),
                    asset_token.val
                );
            }
            let seq_id = rest_client.get_block_commit_count()?;
            match define_asset(
                data_dir,
                seq_id,
                fiat_asset,
                &issuer_key_pair,
                asset_token,
                &memo,
                asset_rules,
                txn_file,
            ) {
                Ok(_) => Ok(()),
                Err(error) => Err(error),
            }
        }
        ("set_kv", Some(kv_matches)) => {
            let data = load_data(data_dir)?;
            let issuer_id = parse_to_u64(
                asset_issuer_matches
                    .value_of("id")
                    .ok_or_else(|| PlatformError::InputsError(error_location!()))?,
            )?;
            let key_pair = data.get_asset_issuer_key_pair(issuer_id)?;
            let key = Key::hash(kv_matches.value_of("key").unwrap());
            let gen = parse_to_u64(
                kv_matches
                    .value_of("gen")
                    .ok_or_else(|| PlatformError::InputsError(error_location!()))?,
            )
            .map_err(|e| {
                PlatformError::InputsError(format!("{}:{}", e, error_location!()))
            })?;
            let value = kv_matches
                .value_of("value")
                .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
            let seq_id = rest_client.get_block_commit_count()?;
            let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
            let hash = KVHash::new(&value, None);
            txn_builder.add_operation_kv_update(&key_pair, &key, gen, Some(&hash))?;
            println!("Hash of data will be stored at key {}", key.to_base64());
            if let Some(txn_file) = txn_file {
                store_txn_to_file(&txn_file, &txn_builder)
            } else {
                inputs_error("Missing --txn <filename>")
            }
        }
        ("clear_kv", Some(kv_matches)) => {
            let data = load_data(data_dir)?;
            let issuer_id = parse_to_u64(
                asset_issuer_matches
                    .value_of("id")
                    .ok_or_else(|| PlatformError::InputsError(error_location!()))?,
            )?;
            let key =
                Key::from_base64(kv_matches.value_of("key").unwrap()).map_err(|e| {
                    PlatformError::InputsError(format!("{}:{}", e, error_location!()))
                })?;
            let key_pair = data.get_asset_issuer_key_pair(issuer_id)?;
            let gen = parse_to_u64(
                kv_matches
                    .value_of("gen")
                    .ok_or_else(|| PlatformError::InputsError(error_location!()))?,
            )
            .map_err(|e| {
                PlatformError::InputsError(format!("{}:{}", e, error_location!()))
            })?;
            let seq_id = rest_client.get_block_commit_count()?;
            let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);

            txn_builder.add_operation_kv_update(&key_pair, &key, gen, None)?;
            if let Some(txn_file) = txn_file {
                store_txn_to_file(&txn_file, &txn_builder)
            } else {
                inputs_error("Missing --txn <filename>")
            }
        }
        ("issue_asset", Some(issue_asset_matches)) => {
            let data = load_data(data_dir)?;
            debug!("issue_asset: entering");
            let (key_pair, _) = if let Some(id_arg) = asset_issuer_matches.value_of("id")
            {
                let issuer_id = parse_to_u64(id_arg)?;
                (
                    data.get_asset_issuer_key_pair(issuer_id)?,
                    data.get_asset_tracer_key_pair(issuer_id)?.enc_key,
                )
            } else {
                eprintln!(
                    "Asset issuer id is required to issue asset. Use asset_issuer --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            debug!("issue_asset: key_pair={:?}", &key_pair);
            let token_code = if let Some(token_code_arg) =
                issue_asset_matches.value_of("token_code")
            {
                AssetTypeCode::new_from_base64(token_code_arg)?
            } else {
                eprintln!("Token code is required to issue asset. Use --token_code.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            debug!("issue_asset: token_code={:?}", &token_code);
            let amount = if let Some(amount_arg) = issue_asset_matches.value_of("amount")
            {
                parse_to_u64(amount_arg)?
            } else {
                eprintln!("Amount is required to issue asset. Use --amount.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let confidential_amount =
                issue_asset_matches.is_present("confidential_amount");
            let seq_id = rest_client.get_block_commit_count()?;
            let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
            let params = PublicParams::new();
            if let Err(e) = txn_builder.add_basic_issue_asset(
                &key_pair,
                &token_code,
                get_and_update_sequence_number(data_dir)?,
                amount,
                AssetRecordType::from_booleans(confidential_amount, false),
                &params,
            ) {
                eprintln!("Failed to add basic issue asset.");
                return Err(e);
            }
            if let Some(txn_file) = txn_file {
                store_txn_to_file(&txn_file, &txn_builder)
            } else {
                inputs_error("Missing --txn <filename>")
            }
        }
        ("transfer_asset", Some(transfer_asset_matches)) => {
            // TODO (redmine issue #36) to support co-signatures we need to use
            // TransferOperationBuilder
            let data = load_data(data_dir)?;
            debug!("transfer_asset: entering");
            let (issuer_key_pair, tracer_enc_keys) = if let Some(id_arg) =
                asset_issuer_matches.value_of("id")
            {
                let issuer_id = parse_to_u64(id_arg)?;
                (
                    data.get_asset_issuer_key_pair(issuer_id)?,
                    data.get_asset_tracer_key_pair(issuer_id)?.enc_key,
                )
            } else {
                eprintln!(
                    "Asset issuer id is required to transfer asset. Use asset_issuer --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            // Compose transfer_from for add_basic_transfer_asset
            debug!("transfer_asset: got keys");
            let mut txo_refs = Vec::new();
            if let Some(sids_file_arg) = transfer_asset_matches.value_of("sids_file") {
                match load_sids_from_file(sids_file_arg) {
                    Ok(sids) => {
                        for sid in sids {
                            txo_refs.push(TxoRef::Absolute(TxoSID(sid)));
                        }
                    }
                    Err(e) => panic!("load_sids_from_file: err={:?}", e),
                }
            } else {
                eprintln!("Sids are required to transfer asset. Use --sids_file.");
                return Err(PlatformError::InputsError(error_location!()));
            }
            debug!("transfer_asset: got txo_refs");
            let bars_and_owner_memos = if let Some(issuance_txn_files_arg) =
                transfer_asset_matches.value_of("issuance_txn_files")
            {
                load_blind_asset_records_and_owner_memos_from_files(
                    issuance_txn_files_arg,
                )?
            } else {
                eprintln!(
                    "Blind asset records and associated memos are required to transfer asset. Use --issuance_txn_files."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            debug!("transfer_asset: got bars_and_owner_memos");
            let tracing_policy = if transfer_asset_matches.is_present("traceable") {
                Some(AssetTracingPolicy {
                    enc_keys: tracer_enc_keys,
                    asset_tracking: true,
                    identity_tracking: None,
                })
            } else {
                None
            };
            let input_amounts = if let Some(input_amounts_arg) =
                transfer_asset_matches.value_of("input_amounts")
            {
                parse_to_u64_vec(input_amounts_arg)?
            } else {
                eprintln!(
                    "Input amounts are required to transfer asset. Use --input_amounts."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let mut count = txo_refs.len();
            if input_amounts.len() != count || bars_and_owner_memos.len() != count {
                eprintln!("Size of input sids and input amounts should match.");
                return Err(PlatformError::InputsError(error_location!()));
            }
            debug!("transfer_asset: got input_amounts");
            let mut transfer_from = Vec::new();
            let mut txo_refs_iter = txo_refs.iter();
            let mut bars_and_owner_memos_iter = bars_and_owner_memos.iter();
            let mut input_amounts_iter = input_amounts.iter();
            let mut input_tracing_policies = Vec::new();
            let mut input_identity_commitments = Vec::new();
            while count > 0 {
                let txo_refs_next = if let Some(txo_ref) = txo_refs_iter.next() {
                    txo_ref
                } else {
                    eprintln!("More txo ref expected.");
                    return Err(PlatformError::InputsError(error_location!()));
                };
                let (blind_asset_record_next, owner_memo_next) =
                    if let Some(bar_and_owner_memo) = bars_and_owner_memos_iter.next() {
                        bar_and_owner_memo
                    } else {
                        eprintln!("More blind asset record and owner memo expected.");
                        return Err(PlatformError::InputsError(error_location!()));
                    };
                let input_amount_next =
                    if let Some(input_amount) = input_amounts_iter.next() {
                        *input_amount
                    } else {
                        eprintln!("More input amount expected.");
                        return Err(PlatformError::InputsError(error_location!()));
                    };
                let transfer_from_next = (
                    txo_refs_next,
                    blind_asset_record_next,
                    input_amount_next,
                    owner_memo_next,
                );
                transfer_from.push(transfer_from_next);
                input_tracing_policies.push(tracing_policy.clone());
                input_identity_commitments.push(None);
                count -= 1;
            }

            // Compose transfer_to for add_basic_transfer_asset
            let mut recipient_addresses = Vec::new();
            if let Some(recipients) = transfer_asset_matches.value_of("recipients") {
                let recipient_ids = parse_to_u64_vec(recipients)?;
                for id in recipient_ids {
                    let recipient_pub_key = data.get_borrower_key_pair(id)?.get_pk();
                    recipient_addresses.push(AccountAddress {
                        key: recipient_pub_key,
                    });
                }
            } else {
                eprintln!(
                    "Recipient ids are required to transfer asset. Use --recipients."
                );
                return Err(PlatformError::InputsError(error_location!()));
            }
            let output_amounts = if let Some(output_amounts_arg) =
                transfer_asset_matches.value_of("output_amounts")
            {
                parse_to_u64_vec(output_amounts_arg)?
            } else {
                eprintln!(
                    "Output amounts are required to transfer asset. Use --output_amounts."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let mut count = output_amounts.len();
            if recipient_addresses.len() != count {
                eprintln!("Size of output amounts and addresses should match.");
                return Err(PlatformError::InputsError(error_location!()));
            }
            let mut transfer_to = Vec::new();
            let mut output_amounts_iter = output_amounts.iter();
            let mut addresses_iter = recipient_addresses.iter();
            let mut output_tracing_policies = Vec::new();
            let mut output_identity_commitments = Vec::new();
            while count > 0 {
                let output_amount_next =
                    if let Some(output_amount) = output_amounts_iter.next() {
                        *output_amount
                    } else {
                        eprintln!("More output amount expected.");
                        return Err(PlatformError::InputsError(error_location!()));
                    };
                let address_next = if let Some(address) = addresses_iter.next() {
                    address
                } else {
                    eprintln!("More address expected.");
                    return Err(PlatformError::InputsError(error_location!()));
                };
                transfer_to.push((output_amount_next, address_next));
                output_tracing_policies.push(tracing_policy.clone());
                output_identity_commitments.push(None);
                count -= 1;
            }

            // Transfer asset
            let seq_id = rest_client.get_block_commit_count()?;
            let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
            if let Err(e) = txn_builder.add_basic_transfer_asset(
                &issuer_key_pair,
                &transfer_from[..],
                input_tracing_policies,
                input_identity_commitments,
                &transfer_to[..],
                output_tracing_policies,
                output_identity_commitments,
            ) {
                eprintln!("Failed to add operation to transaction.");
                return Err(e);
            };
            debug!("transfer_asset: about to store file");
            if let Some(txn_file) = txn_file {
                store_txn_to_file(&txn_file, &txn_builder)
            } else {
                inputs_error("Missing --txn <filename>")
            }
        }
        ("issue_and_transfer_asset", Some(issue_and_transfer_matches)) => {
            let data = load_data(data_dir)?;
            let issuer_key_pair = if let Some(id_arg) =
                asset_issuer_matches.value_of("id")
            {
                let issuer_id = parse_to_u64(id_arg)?;
                data.get_asset_issuer_key_pair(issuer_id)?
            } else {
                eprintln!(
                    "Asset issuer id is required to issue and transfer asset. Use asset_issuer --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let recipient_key_pair = if let Some(id_arg) =
                issue_and_transfer_matches.value_of("recipient")
            {
                let recipient_id = parse_to_u64(id_arg)?;
                data.get_borrower_key_pair(recipient_id)?
            } else {
                eprintln!(
                    "Recipient id is required to issue and transfer asset. Use --recipient."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let amount = if let Some(amount_arg) =
                issue_and_transfer_matches.value_of("amount")
            {
                parse_to_u64(amount_arg)?
            } else {
                eprintln!(
                    "Amount is required to issue and transfer asset. Use --amount."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let token_code = if let Some(token_code_arg) =
                issue_and_transfer_matches.value_of("token_code")
            {
                AssetTypeCode::new_from_base64(token_code_arg)?
            } else {
                eprintln!("Token code is required to issue asset. Use --token_code.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let confidential_amount =
                issue_and_transfer_matches.is_present("confidential_amount");
            let record_type = AssetRecordType::from_booleans(confidential_amount, false);
            let memo_file = issue_and_transfer_matches.value_of("memo_file");
            let seq_id = rest_client.get_block_commit_count()?;
            issue_and_transfer_asset(
                data_dir,
                seq_id,
                &issuer_key_pair,
                &recipient_key_pair,
                amount,
                token_code,
                record_type,
                None,
                txn_file,
                memo_file,
                None,
                None,
            )?;
            Ok(())
        }
        ("trace_and_verify_asset", Some(trace_and_verify_asset_matches)) => {
            let data = load_data(data_dir)?;
            debug!(
                "trace_and_verify_asset_matches = {:?}",
                &trace_and_verify_asset_matches
            );
            let tracer_dec_keys = if let Some(id_arg) =
                asset_issuer_matches.value_of("id")
            {
                let issuer_id = parse_to_u64(id_arg)?;
                data.get_asset_tracer_key_pair(issuer_id)?
                    .dec_key
                    .record_data_dec_key
            } else {
                eprintln!(
                    "Asset issuer id is required to trace the asset. Use asset_issuer --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let tracer_and_owner_memos = if let Some(memo_file_arg) =
                trace_and_verify_asset_matches.value_of("memo_file")
            {
                load_tracer_and_owner_memos_from_files(memo_file_arg)?
            } else {
                eprintln!("Owner memo is required to trace the asset. Use --memo_file.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let tracer_memo = if let Some(memo) = tracer_and_owner_memos[0].0.get(0) {
                memo
            } else {
                eprintln!("The asset isn't traceable.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let expected_amount = if let Some(expected_amount_arg) =
                trace_and_verify_asset_matches.value_of("expected_amount")
            {
                parse_to_u64(expected_amount_arg)?
            } else {
                eprintln!(
                    "Expected amount is required to verify the asset. Use --expected_amount."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            tracer_memo
                .verify_amount(&tracer_dec_keys, expected_amount)
                .map_err(|error| PlatformError::ZeiError(error_location!(), error))
        }
        ("trace_credential", Some(trace_credential_matches)) => {
            let data = load_data(data_dir)?;
            let attrs_dec_key = if let Some(id_arg) = asset_issuer_matches.value_of("id")
            {
                let issuer_id = parse_to_u64(id_arg)?;
                let asset_tracer_key_pair = data.get_asset_tracer_key_pair(issuer_id)?;
                asset_tracer_key_pair.dec_key.attrs_dec_key
            } else {
                eprintln!(
                    "Asset issuer id is required to trace the asset. Use asset_issuer --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let tracer_memo = if let Some(memo_file_arg) =
                trace_credential_matches.value_of("memo_file")
            {
                load_tracer_memo_from_file(memo_file_arg)?
            } else {
                eprintln!(
                    "Tracer memo is required to trace the credential. Use --memo_file."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let len = if let Some(attribute_arg) =
                trace_credential_matches.value_of("attribute")
            {
                let credential_issuer_public_key =
                    data.get_credential_issuer_key_pair(0)?.0;
                credential_issuer_public_key
                    .get_len(attribute_arg)
                    .map_err(|e| PlatformError::ZeiError(error_location!(), e))?
            } else {
                eprintln!(
                    "Credential attribute is required to verify the credential. Use --attribute."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let expected_value = if let Some(expected_value_arg) =
                trace_credential_matches.value_of("expected_value")
            {
                u8_slice_to_u32_vec(expected_value_arg.as_bytes(), len)
            } else {
                eprintln!(
                    "Expected value is required to verify the credential. Use --expected_value."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            match tracer_memo.verify_identity_attributes(&attrs_dec_key, &expected_value)
            {
                Ok(res) => {
                    if res[0] {
                        debug!("Credential verification succeeded.");
                    } else {
                        eprintln!("Credential value isn't as expected.");
                        return Err(PlatformError::InputsError(error_location!()));
                    }
                    Ok(())
                }
                Err(e) => Err(PlatformError::ZeiError(error_location!(), e)),
            }
        }
        _ => {
            eprintln!(
                "Subcommand missing or not recognized. Try \"txn_cli asset_issuer --help\""
            );
            Err(PlatformError::InputsError(error_location!()))
        }
    }
}

/// Processes the `credential_issuer` subcommand.
///
/// Subcommands under `credential_issuer`
/// * `sign_up`
///
/// # Arguments
/// * `credential_issuer_matches`: subcommands and arguments under the `credential_issuer` subcommand.
pub(crate) fn process_credential_issuer_cmd(
    credential_issuer_matches: &clap::ArgMatches,
    data_dir: &str,
) -> Result<(), PlatformError> {
    match credential_issuer_matches.subcommand() {
        ("sign_up", Some(sign_up_matches)) => {
            let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
                name_arg.to_owned()
            } else {
                eprintln!(
                    "Name is required to sign up a credential issuer account. Use --name."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let mut attributes = Vec::new();
            if sign_up_matches.is_present("min_credit_score") {
                attributes.push(CredentialIndex::MinCreditScore);
            }
            if sign_up_matches.is_present("min_income") {
                attributes.push(CredentialIndex::MinIncome);
            }
            if sign_up_matches.is_present("citizenship") {
                attributes.push(CredentialIndex::Citizenship);
            }
            let mut data = load_data(data_dir)?;
            data.add_credential_issuer(data_dir, name, attributes)
        }
        _ => {
            eprintln!(
                "Subcommand missing or not recognized. Try \"txn_cli credential_issuer --help\""
            );
            Err(PlatformError::InputsError(error_location!()))
        }
    }
}

/// Processes the `lender` subcommand.
///
/// Subcommands under `lender`
/// * `sign_up`
/// * `view_loan`
/// * `fulfill_loan`
///
/// # Arguments
/// * `lender_matches`: subcommands and arguments under the `lender` subcommand.
pub(crate) fn process_lender_cmd<T: RestfulLedgerAccess + RestfulLedgerUpdate>(
    lender_matches: &clap::ArgMatches,
    data_dir: &str,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    let mut data = load_data(data_dir)?;
    match lender_matches.subcommand() {
        ("sign_up", Some(sign_up_matches)) => {
            let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
                name_arg.to_owned()
            } else {
                eprintln!("Name is required to sign up a lender account. Use --name.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            data.add_lender(data_dir, name)
        }
        ("view_loan", Some(view_loan_matches)) => {
            let lender_id = if let Some(id_arg) = lender_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Lender id is required to get loan information. Use lender --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            if let Some(loan_arg) = view_loan_matches.value_of("loan") {
                let loan_id = parse_to_u64(loan_arg)?;
                let loan = data.loans[loan_id as usize].clone();
                if loan.lender != lender_id {
                    eprintln!("Lender {} doesn't own loan {}.", lender_id, loan_id);
                    return Err(PlatformError::InputsError(error_location!()));
                }
                debug!("Displaying loan {}: {:?}.", loan_id, loan);
                return Ok(());
            }
            let mut loans = Vec::new();
            let loan_ids = data.lenders[lender_id as usize].loans.clone();
            if let Some(filter) = view_loan_matches.value_of("filter") {
                for id in loan_ids {
                    match filter {
                        "requested" => {
                            if data.loans[id as usize].status == LoanStatus::Requested {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "fulfilled" => {
                            if data.loans[id as usize].status == LoanStatus::Active
                                || data.loans[id as usize].status == LoanStatus::Complete
                            {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "declined" => {
                            if data.loans[id as usize].status == LoanStatus::Declined {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "active" => {
                            if data.loans[id as usize].status == LoanStatus::Active {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "complete" => {
                            if data.loans[id as usize].status == LoanStatus::Complete {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        _ => {
                            loans.push(data.loans[id as usize].clone());
                        }
                    }
                }
            } else {
                for id in loan_ids {
                    loans.push(data.loans[id as usize].clone());
                }
            }
            debug!("Displaying {} loan(s): {:?}", loans.len(), loans);
            Ok(())
        }
        ("fulfill_loan", Some(fulfill_loan_matches)) => {
            let loan_id = if let Some(loan_arg) = fulfill_loan_matches.value_of("loan") {
                parse_to_u64(loan_arg)?
            } else {
                eprintln!("Loan id is required to fulfill the loan. Use --loan.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            if let Some(id_arg) = lender_matches.value_of("id") {
                let lender_id = parse_to_u64(id_arg)?;
                let loan = data.loans[loan_id as usize].clone();
                if loan.lender != lender_id {
                    eprintln!("Lender {} doesn't own loan {}.", lender_id, loan_id);
                    return Err(PlatformError::InputsError(error_location!()));
                }
            } else {
                eprintln!("Lender id is required to fulfill a loan. Use lender --id.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let issuer_id =
                if let Some(issuer_arg) = fulfill_loan_matches.value_of("issuer") {
                    parse_to_u64(issuer_arg)?
                } else {
                    eprintln!(
                        "Asset issuer id is required to fulfill the loan. Use --issuer."
                    );
                    return Err(PlatformError::InputsError(error_location!()));
                };
            let memo_file = fulfill_loan_matches.value_of("memo_file");
            fulfill_loan(data_dir, loan_id, issuer_id, memo_file, rest_client)
        }
        (
            "create_or_overwrite_requirement",
            Some(create_or_overwrite_requirement_matches),
        ) => {
            let lender_id = if let Some(id_arg) = lender_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Lender id is required to get credential requirement information. Use lender --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let attribute = if let Some(attribute_arg) =
                create_or_overwrite_requirement_matches.value_of("attribute")
            {
                match attribute_arg {
                    "min_credit_score" => CredentialIndex::MinCreditScore,
                    "min_income" => CredentialIndex::MinIncome,
                    _ => CredentialIndex::Citizenship,
                }
            } else {
                eprintln!(
                    "Credential attribute is required to create or overwrite the credential requirement. Use --attribute."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let requirement = if let Some(requirement_arg) =
                create_or_overwrite_requirement_matches.value_of("requirement")
            {
                requirement_arg
            } else {
                eprintln!(
                    "Credential value is required to create or overwrite the credential requirement. Use --requirement."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let mut data = load_data(data_dir)?;
            data.create_or_overwrite_requirement(
                data_dir,
                lender_id,
                attribute,
                requirement,
            )
        }
        _ => {
            eprintln!(
                "Subcommand missing or not recognized. Try \"txn_cli lender --help\""
            );
            Err(PlatformError::InputsError(error_location!()))
        }
    }
}

/// Processes the `borrower` subcommand.
///
/// Subcommands under `borrower`
/// * `sign_up`
/// * `load_funds`
/// * `view_loan`
/// * `request_loan`
/// * `pay_loan`
/// * `view_credential`
/// * `create_or_overwrite_credential`
/// * `get_asset_record`
///
/// # Arguments
/// * `borrower_matches`: subcommands and arguments under the `borrower` subcommand.
pub(crate) fn process_borrower_cmd<
    T: RestfulQueryServerAccess + RestfulLedgerAccess + RestfulLedgerUpdate,
>(
    borrower_matches: &clap::ArgMatches,
    data_dir: &str,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    let mut data = load_data(data_dir)?;
    match borrower_matches.subcommand() {
        ("sign_up", Some(sign_up_matches)) => {
            let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
                name_arg.to_owned()
            } else {
                eprintln!("Name is required to sign up a lender account. Use --name.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            data.add_borrower(data_dir, name)
        }
        ("load_funds", Some(load_funds_matches)) => {
            let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!("Borrower id is required to load funds. Use borrower --id.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            process_load_funds_cmd(
                load_funds_matches,
                data_dir,
                borrower_id,
                rest_client,
            )
        }
        ("view_loan", Some(view_loan_matches)) => {
            let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Borrower id is required to get loan information. Use borrower --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            if let Some(loan_arg) = view_loan_matches.value_of("loan") {
                let loan_id = parse_to_u64(loan_arg)?;
                let loan = data.loans[loan_id as usize].clone();
                if loan.borrower != borrower_id {
                    eprintln!("Borrower {} doesn't own loan {}.", borrower_id, loan_id);
                    return Err(PlatformError::InputsError(error_location!()));
                }
                debug!("Displaying loan {}: {:?}.", loan_id, loan);
                return Ok(());
            }
            let mut loans = Vec::new();
            let loan_ids = data.borrowers[borrower_id as usize].loans.clone();
            if let Some(filter) = view_loan_matches.value_of("filter") {
                for id in loan_ids {
                    match filter {
                        "requested" => {
                            if data.loans[id as usize].status == LoanStatus::Requested {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "fulfilled" => {
                            if data.loans[id as usize].status == LoanStatus::Active
                                || data.loans[id as usize].status == LoanStatus::Complete
                            {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "declined" => {
                            if data.loans[id as usize].status == LoanStatus::Declined {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "active" => {
                            if data.loans[id as usize].status == LoanStatus::Active {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        "complete" => {
                            if data.loans[id as usize].status == LoanStatus::Complete {
                                loans.push(data.loans[id as usize].clone());
                            }
                        }
                        _ => {
                            loans.push(data.loans[id as usize].clone());
                        }
                    }
                }
            } else {
                for id in loan_ids {
                    loans.push(data.loans[id as usize].clone());
                }
            }
            debug!("Displaying {} loan(s): {:?}", loans.len(), loans);
            Ok(())
        }
        ("request_loan", Some(request_loan_matches)) => {
            let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Borrower id is required to request a loan. Use borrower --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let lender_id = if let Some(lender_arg) =
                request_loan_matches.value_of("lender")
            {
                parse_to_u64(lender_arg)?
            } else {
                eprintln!("Lender id is required to request the loan. Use --lender.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let amount =
                if let Some(amount_arg) = request_loan_matches.value_of("amount") {
                    parse_to_u64(amount_arg)?
                } else {
                    eprintln!("Amount is required to request the loan. Use --amount.");
                    return Err(PlatformError::InputsError(error_location!()));
                };
            let interest_per_mille = if let Some(interest_per_mille_arg) =
                request_loan_matches.value_of("interest_per_mille")
            {
                parse_to_u64(interest_per_mille_arg)?
            } else {
                eprintln!(
                    "Interest per mille is required to request the loan. Use --interest_per_mille."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let duration =
                if let Some(duration_arg) = request_loan_matches.value_of("duration") {
                    parse_to_u64(duration_arg)?
                } else {
                    eprintln!("Duration is required to request the loan. Use --amount.");
                    return Err(PlatformError::InputsError(error_location!()));
                };
            let mut data = load_data(data_dir)?;
            data.add_loan(
                data_dir,
                lender_id,
                borrower_id,
                amount,
                interest_per_mille,
                duration,
            )
        }
        ("pay_loan", Some(pay_loan_matches)) => {
            let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Borrower id is required to pay off the loan. Use borrower --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            if let Some(loan_arg) = pay_loan_matches.value_of("loan") {
                let loan_id = parse_to_u64(loan_arg)?;
                let loan = data.loans[loan_id as usize].clone();
                if loan.borrower != borrower_id {
                    eprintln!("Borrower {} doesn't own loan {}.", borrower_id, loan_id);
                    return Err(PlatformError::InputsError(error_location!()));
                }
            } else {
                eprintln!("Loan id is required to pay the loan.");
                return Err(PlatformError::InputsError(error_location!()));
            }
            process_pay_loan_cmd(pay_loan_matches, data_dir, rest_client)
        }
        ("view_credential", Some(view_credential_matches)) => {
            let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Borrower id is required to get credential information. Use borrower --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let credential_id = if let Some(id) =
                data.borrowers[borrower_id as usize].credentials
            {
                id
            } else {
                debug!(
                    "No credential is found. Use create_or_overwrite_credential to create a credential record."
                );
                return Ok(());
            };
            if let Some(attribute_arg) = view_credential_matches.value_of("attribute") {
                let attribute = match attribute_arg {
                    "min_credit_score" => CredentialIndex::MinCreditScore,
                    "min_income" => CredentialIndex::MinIncome,
                    _ => CredentialIndex::Citizenship,
                };
                let value = data.credentials[credential_id as usize].values
                    [attribute as usize]
                    .clone();
                debug!("Displaying {:?}: {:?}", attribute.get_name(), value);
            } else {
                debug!("Displaying credentials:");
                let values = data.credentials[credential_id as usize].values.clone();
                for attribute in [
                    CredentialIndex::MinCreditScore,
                    CredentialIndex::MinIncome,
                    CredentialIndex::Citizenship,
                ]
                .iter()
                {
                    if let Some(value) = values[*attribute as usize].clone() {
                        debug!("{}: {}.", attribute.get_name(), value);
                    }
                }
            };
            Ok(())
        }
        (
            "create_or_overwrite_credential",
            Some(create_or_overwrite_credential_matches),
        ) => {
            let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Borrower id is required to get credential information. Use borrower --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let credential_issuer_id = if let Some(credential_issuer_arg) =
                create_or_overwrite_credential_matches.value_of("credential_issuer")
            {
                parse_to_u64(credential_issuer_arg)?
            } else {
                eprintln!(
                    "Credential issuer id is required to get credential information. Use --credential_issuer."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let attribute = if let Some(attribute_arg) =
                create_or_overwrite_credential_matches.value_of("attribute")
            {
                match attribute_arg {
                    "min_credit_score" => CredentialIndex::MinCreditScore,
                    "min_income" => CredentialIndex::MinIncome,
                    _ => CredentialIndex::Citizenship,
                }
            } else {
                eprintln!(
                    "Credential attribute is required to create or overwrite the credential. Use --attribute."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let value = if let Some(value_arg) =
                create_or_overwrite_credential_matches.value_of("value")
            {
                value_arg
            } else {
                eprintln!(
                    "Credential value is required to create or overwrite the credential. Use --value."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let mut data = load_data(data_dir)?;
            data.create_or_overwrite_credential(
                data_dir,
                borrower_id,
                credential_issuer_id,
                attribute,
                value,
            )
        }
        ("get_asset_record", Some(get_asset_record_matches)) => {
            let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
                parse_to_u64(id_arg)?
            } else {
                eprintln!(
                    "Borrower id is required to get the asset record. Use borrower --id."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let data = load_data(data_dir)?;
            let borrower_name = data.borrowers[borrower_id as usize].name.clone();
            let key_pair = data.get_borrower_key_pair(borrower_id)?;
            let sid = if let Some(sid_arg) = get_asset_record_matches.value_of("sid") {
                TxoSID(parse_to_u64(sid_arg)?)
            } else {
                eprintln!(
                    "Sid is required to get the asset record. Use borrower --sid."
                );
                return Err(PlatformError::InputsError(error_location!()));
            };
            let owner_memo = rest_client.get_owner_memo(sid.0)?;
            let asset_record =
                query_open_asset_record(rest_client, sid, &key_pair, &owner_memo)?;
            println!(
                "{} owns {} of asset {:?}.",
                borrower_name,
                asset_record.get_amount(),
                asset_record.get_asset_type()
            );
            Ok(())
        }
        _ => {
            eprintln!(
                "Subcommand missing or not recognized. Try \"txn_cli borrower --help\""
            );
            Err(PlatformError::InputsError(error_location!()))
        }
    }
}

/// Processes the `create_txn_builder` subcommand.
/// # Arguments
/// * `create_matches`: subcommands and arguments under the `create_txn_builder` subcommand.
/// * `txn_file`: path to store the transaction file.
pub(crate) fn process_create_txn_builder_cmd<
    T: RestfulLedgerAccess + RestfulLedgerUpdate,
>(
    create_matches: &clap::ArgMatches,
    txn_file: &str,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    let name = create_matches.value_of("name");
    let overwrite = create_matches.is_present("overwrite");
    let file_str = if let Some(name) = name {
        name.to_string()
    } else {
        txn_file.to_string()
    };
    let expand_str = shellexpand::tilde(&file_str).to_string();
    create_directory_and_rename_path(&expand_str, overwrite)?;
    let seq_id = rest_client.get_block_commit_count()?;
    let txn_builder = TransactionBuilder::from_seq_id(seq_id);
    store_txn_to_file(&expand_str, &txn_builder)
}

/// Processes the `submit` subcommand.
/// # Arguments
/// * `submit_matches`: subcommands and arguments under the `submit` subcommand.
/// * `txn_file`: path to store the transaction file.
pub(crate) fn process_submit_cmd<T: RestfulLedgerUpdate>(
    submit_matches: &clap::ArgMatches,
    txn_file: &str,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    if let Ok(txn_builder) = load_txn_from_file(txn_file) {
        if submit_matches.is_present("get_sids")
            || submit_matches.is_present("sids_file")
        {
            let sids = submit_and_get_sids(rest_client, txn_builder)?;
            println!("Utxo: {:?}", sids);
            if let Some(path) = submit_matches.value_of("sids_file") {
                let mut sids_str = "".to_owned();
                for sid in sids {
                    sids_str.push_str(&format!("{},", sid.0));
                }
                store_sids_to_file(path, &sids_str)?;
            }
            Ok(())
        } else {
            rest_client.submit_transaction(&txn_builder.transaction())?;
            Ok(())
        }
    } else {
        let msg = format!("couldn't load transaction from {}", txn_file);
        eprintln!("{}", &msg);
        io_error(&msg)
    }
}

/// Processes the `borrower load_funds` subcommand.
/// # Arguments
/// * `borrower_id`: borrower ID.
/// * `load_funds_matches`: subcommands and arguments under the `load_funds` subcommand.
pub(crate) fn process_load_funds_cmd<T: RestfulLedgerAccess + RestfulLedgerUpdate>(
    load_funds_matches: &clap::ArgMatches,
    data_dir: &str,
    borrower_id: u64,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    let issuer_id = if let Some(issuer_arg) = load_funds_matches.value_of("issuer") {
        if let Ok(id) = issuer_arg.parse::<u64>() {
            id
        } else {
            eprintln!("Improperly formatted issuer id.");
            return Err(PlatformError::InputsError(error_location!()));
        }
    } else {
        eprintln!("Asset issuer id is required to load funds. Use --issuer.");
        return Err(PlatformError::InputsError(error_location!()));
    };
    let amount = if let Some(amount_arg) = load_funds_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
    } else {
        eprintln!("Amount is required to load funds. Use --amount.");
        return Err(PlatformError::InputsError(error_location!()));
    };
    load_funds(data_dir, issuer_id, borrower_id, amount, rest_client)
}

/// Processes the `borrower pay_loan` subcommand.
/// # Arguments
/// * `pay_loan_matches`: subcommands and arguments under the `pay_loan` subcommand.

pub(crate) fn process_pay_loan_cmd<T: RestfulLedgerAccess + RestfulLedgerUpdate>(
    pay_loan_matches: &clap::ArgMatches,
    data_dir: &str,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    let loan_id = if let Some(loan_arg) = pay_loan_matches.value_of("loan") {
        parse_to_u64(loan_arg)?
    } else {
        eprintln!("Loan id is required to pay the loan. Use --loan.");
        return Err(PlatformError::InputsError(error_location!()));
    };
    let amount = if let Some(amount_arg) = pay_loan_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
    } else {
        eprintln!("Amount is required to pay the loan. Use --amount.");
        return Err(PlatformError::InputsError(error_location!()));
    };

    pay_loan(data_dir, loan_id, amount, rest_client)
}

/// Processes input commands and arguments.
/// # Arguments
/// * `inputs`: input subcommands and arguments.
pub fn process_inputs<
    T: RestfulQueryServerAccess + RestfulLedgerAccess + RestfulLedgerUpdate,
>(
    inputs: clap::ArgMatches,
    rest_client: &mut T,
) -> Result<(), PlatformError> {
    let dir = if let Some(dir) = inputs.value_of("dir") {
        dir.to_string()
    } else if let Ok(dir) = env::var("FINDORA_DIR") {
        dir
    } else {
        let home_dir = if let Some(dir) = dirs::home_dir() {
            dir
        } else {
            return Err(PlatformError::IoError(
                "Failed to get the home directory.".to_owned(),
            ));
        };
        let dir_str = if let Some(string) = home_dir.to_str() {
            string
        } else {
            return Err(PlatformError::IoError(
                "Failed to convert the path to string.".to_owned(),
            ));
        };
        format!("{}/.findora", dir_str)
    };

    match inputs.subcommand() {
        ("asset_issuer", Some(asset_issuer_matches)) => {
            let txn_file_opt = inputs.value_of("txn");
            process_asset_issuer_cmd(
                asset_issuer_matches,
                &dir,
                txn_file_opt,
                rest_client,
            )
        }
        ("credential_issuer", Some(credential_issuer_matches)) => {
            process_credential_issuer_cmd(credential_issuer_matches, &dir)
        }
        ("lender", Some(issuer_matches)) => {
            process_lender_cmd(issuer_matches, &dir, rest_client)
        }
        ("borrower", Some(issuer_matches)) => {
            process_borrower_cmd(issuer_matches, &dir, rest_client)
        }
        ("create_txn_builder", Some(create_txn_builder_matches)) => {
            if let Some(txn_file) = create_txn_builder_matches.value_of("name") {
                process_create_txn_builder_cmd(
                    create_txn_builder_matches,
                    &txn_file,
                    rest_client,
                )
            } else {
                eprintln!("Missing --name <filename>");
                inputs_error(&format!(
                    "Missing --name <filename> at {}",
                    error_location!()
                ))
            }
        }
        ("serialize", Some(_serialize_matches)) => {
            if let Some(txn_file) = inputs.value_of("txn") {
                let txn_builder = load_txn_from_file(&txn_file).map_err(|e| {
                    eprintln!("File {} is not a valid transaction file.", txn_file);
                    e
                })?;
                match serde_json::to_string(txn_builder.transaction()) {
                    Ok(as_json) => {
                        debug!("{}", as_json);
                        Ok(())
                    }
                    Err(_) => {
                        eprintln!("Failed to serialize txn.");
                        Err(ser_fail!())
                    }
                }
            } else {
                eprintln!("Missing --txn <filename>");
                inputs_error(&format!(
                    "Missing --txn <filename> at {}",
                    error_location!()
                ))
            }
        }
        ("submit", Some(submit_matches)) => {
            if let Some(txn_file) = inputs.value_of("txn") {
                process_submit_cmd(submit_matches, &txn_file, rest_client)
            } else {
                eprintln!("Missing --txn <filename>");
                inputs_error(&format!(
                    "Missing --txn <filename> at {}",
                    error_location!()
                ))
            }
        }
        e => {
            eprintln!(
                "Subcommand {:?} missing or not recognized. Try \"txn_cli --help\"",
                e
            );
            inputs_error(&format!(
                "Subcommand missing or not recognized at {}",
                error_location!()
            ))
        }
    }
}

pub fn get_cli_app<'a, 'b>() -> App<'a, 'b> {
    App::new("Transaction Builder")
    .version(concat!("0.0.1 Build: ",
                     env!("VERGEN_SHA_SHORT"),
                     " ",
                     env!("VERGEN_BUILD_DATE"),
                     "\n",
                     "Copyright 2020 © Findora. All rights reserved."))
    .about("Command line tool for interacting with Findora ledger")
    .arg(Arg::with_name("config")
      .short("c")
      .long("config")
      .value_name("PATH/TO/FILE")
      .help("Specify a custom config file (default: \"$FINDORA_DIR/config.toml\")")
      .takes_value(true))
    .arg(Arg::with_name("local")
      .long("local")
      .help("If local flag is specified, transactions will be submitted to a local ledger"))
    .arg(Arg::with_name("dir")
      .short("d")
      .long("dir")
      .value_name("PATH")
      .help("Directory for configuaration, security, and temporary files; must be writable")
      .takes_value(true)
      .env("FINDORA_DIR"))
    .arg(Arg::with_name("txn")
      .long("txn")
      .value_name("FILE")
      .help("Use named transaction file (will always be under findora_dir)")
      .takes_value(true))
    .subcommand(SubCommand::with_name("asset_issuer")
      .subcommand(SubCommand::with_name("sign_up")
        .arg(Arg::with_name("name")
          .short("n")
          .long("name")
          .required(true)
          .takes_value(true)
          .help("Asset issuer's name.")))
      .arg(Arg::with_name("id")
        .short("i")
        .long("id")
        .takes_value(true)
        .help("Asset issuer id."))
      .subcommand(SubCommand::with_name("store_sids")
        .arg(Arg::with_name("file")
          .short("f")
          .long("file")
          .required(true)
          .takes_value(true)
          .help("Path to store the sids."))
        .arg(Arg::with_name("indices")
          .short("i")
          .long("indices")
          .required(true)
          .takes_value(true)
          .help("Sids. Separate by comma (\",\").")))
      .subcommand(SubCommand::with_name("store_memos")
        .arg(Arg::with_name("file")
          .short("f")
          .long("file")
          .required(true)
          .takes_value(true)
          .help("Path to store the tracer and owner memos."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Asset amount."))
        .arg(Arg::with_name("confidential_amount")
          .short("m")
          .long("confidential_amount")
          .takes_value(false)
          .help("If specified, the amount will be confidential."))
        .arg(Arg::with_name("token_code")
          .short("t")
          .long("token_code")
          .required(true)
          .takes_value(true)
          .help("Asset token code.")))
      .subcommand(SubCommand::with_name("define_asset")
        .arg(Arg::with_name("fiat")
          .short("f")
          .long("fiat")
          .takes_value(false)
          .help("Indicate the asset is a fiat asset."))
        .arg(Arg::with_name("token_code")
          .long("token_code")
          .short("c")
          .help("Explicit 16 character token code for the new asset; must be a unique name. If specified code is already in use, transaction will fail. If not specified, will display automatically generated token code.")
          .takes_value(true))
        .arg(Arg::with_name("traceable")
          .short("trace")
          .long("traceable")
          .help("If specified, asset transfers can be traced by the issuer "))
        .arg(Arg::with_name("max_units")
          .takes_value(true)
          .long("max_units")
          .help("Set a cap on the total number of units of this asset that can be issued."))
        .arg(Arg::with_name("non_transferable")
          .long("non_transferable")
          .help("Non-transferable assets can only be transferred by the asset issuer."))
        .arg(Arg::with_name("updatable")
          .long("updatable")
          .help("Issuers of updatable assets can change their memos."))
        .arg(Arg::with_name("cosigners")
          .long("cosigners")
          .takes_value(true)
          .help("Ids of cosigners. Cosigners are additional signers needed for asset transfers."))
        .arg(Arg::with_name("cosignature_weights")
          .long("cosignature_weights")
          .takes_value(true)
          .help("Weights of each cosignature key, separated by comma. If unspecified, defaults to 1 for each cosigner."))
        .arg(Arg::with_name("threshold")
          .long("threshold")
          .takes_value(true)
          .help("Minimum sum of cosigner weights for a valid transfer. Default is the number of cosigners."))
        .arg(Arg::with_name("memo")
          .short("m")
          .long("memo")
          .required(true)
          .takes_value(true)
          .help("Memo as Json, with escaped quotation marks"))
        .arg(Arg::with_name("confidential")
          .short("xx")
          .long("confidential")
          .help("Make memo confidential"))
        .arg(Arg::with_name("with_policy")
          .short("p")
          .help("TODO: add support for policies")))
      .subcommand(SubCommand::with_name("issue_asset")
        .arg(Arg::with_name("token_code")
          .short("c")
          .long("token_code")
          .required(true)
          .takes_value(true)
          .help("Token code of the asset to be issued. The transaction will fail if no asset with the token code exists."))
        .arg(Arg::with_name("amount")
          .short("amt")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount of tokens to issue."))
        .arg(Arg::with_name("confidential_amount")
          .short("m")
          .long("confidential_amount")
          .takes_value(false)
          .help("If specified, the amount will be confidential.")))
      .subcommand(SubCommand::with_name("clear_kv")
        .arg(Arg::with_name("key")
          .short("k")
          .long("key")
          .required(true)
          .takes_value(true)
          .help("Which KV-store entry to clear"))
        .arg(Arg::with_name("gen")
          .short("g")
          .long("gen")
          .required(true)
          .takes_value(true)
          .help("Which generation of `key` this is"))
        )
      .subcommand(SubCommand::with_name("set_kv")
        .arg(Arg::with_name("key")
          .short("k")
          .long("key")
          .required(true)
          .takes_value(true)
          .help("Which KV-store entry to set. String passed in will be converted to a base-64 encoded key."))
        .arg(Arg::with_name("gen")
          .short("g")
          .long("gen")
          .required(true)
          .takes_value(true)
          .help("Which generation of `key` this is"))
        .arg(Arg::with_name("value")
          .short("v")
          .long("value")
          .required(true)
          .takes_value(true)
          .help("Data to commit to."))
        )
      .subcommand(SubCommand::with_name("transfer_asset")
        .arg(Arg::with_name("recipients")
          .short("r")
          .long("recipients")
          .required(true)
          .takes_value(true)
          .help("Recipients' ids. Separate by comma (\",\")."))
        .arg(Arg::with_name("sids_file")
          .short("s")
          .long("sids_file")
          .required(true)
          .takes_value(true)
          .help("Path to the input sids."))
        .arg(Arg::with_name("issuance_txn_files")
          .short("f")
          .long("issuance_txn_files")
          .required(true)
          .takes_value(true)
          .help("Paths to the asset issuance transactions."))
        .arg(Arg::with_name("traceable")
          .short("t")
          .long("traceable")
          .help("If specified, the asset will be traceable."))
        .arg(Arg::with_name("input_amounts")
          .short("iamts")
          .long("input_amounts")
          .required(true)
          .takes_value(true)
          .help("Amount to transfer from each record. Separate by comma (\",\")."))
        .arg(Arg::with_name("output_amounts")
          .short("oamts")
          .long("output_amounts")
          .required(true)
          .takes_value(true)
          .help("Amount to transfer to each account. Separate by comma (\",\").")))
      .subcommand(SubCommand::with_name("issue_and_transfer_asset")
        .arg(Arg::with_name("recipient")
          .short("r")
          .long("recipient")
          .required(true)
          .takes_value(true)
          .help("Recipient's id."))
        .arg(Arg::with_name("amount")
          .short("amt")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount of tokens to issue and transfer."))
        .arg(Arg::with_name("token_code")
          .short("tc")
          .long("token_code")
          .required(true)
          .takes_value(true)
          .help("Token code of the asset."))
        .arg(Arg::with_name("memo_file")
          .short("f")
          .long("memo_file")
          .takes_value(true)
          .help("If specified, will store the asset tracer memo and owner memo."))
        .arg(Arg::with_name("confidential_amount")
          .short("m")
          .long("confidential_amount")
          .takes_value(false)
          .help("If specified, the amount will be confidential."))
        .arg(Arg::with_name("confidential_asset")
          .short("s")
          .long("confidential_asset")
          .takes_value(false)
          .help("If specified, the asset transfer will be confidential.")))
      .subcommand(SubCommand::with_name("trace_and_verify_asset")
        .arg(Arg::with_name("memo_file")
          .short("f")
          .long("memo_file")
          .required(true)
          .takes_value(true)
          .help("Path to the tracer and owner memos."))
        .arg(Arg::with_name("expected_amount")
          .short("a")
          .long("expected_amount")
          .required(true)
          .takes_value(true)
          .help("Expected asset amount to verify.")))
      .subcommand(SubCommand::with_name("trace_credential")
        .arg(Arg::with_name("memo_file")
          .short("f")
          .long("memo_file")
          .required(true)
          .takes_value(true)
          .help("Path to the tracer and owner memos."))
        .arg(Arg::with_name("attribute")
          .short("a")
          .long("attribute")
          .required(true)
          .takes_value(true)
          .possible_values(&["min_credit_score", "min_income", "citizenship"])
          .help("Credential attribute to verify."))
        .arg(Arg::with_name("expected_value")
          .short("v")
          .long("expected_value")
          .required(true)
          .takes_value(true)
          .help("Expected credential value to verify."))))
    .subcommand(SubCommand::with_name("credential_issuer")
      .subcommand(SubCommand::with_name("sign_up")
        .arg(Arg::with_name("name")
          .short("n")
          .long("name")
          .required(true)
          .takes_value(true)
          .help("Credential issuer's name.")))
      .arg(Arg::with_name("id")
        .short("i")
        .long("id")
        .takes_value(true)
        .help("Credential issuer id.")))
    .subcommand(SubCommand::with_name("lender")
      .subcommand(SubCommand::with_name("sign_up")
        .arg(Arg::with_name("name")
          .short("n")
          .long("name")
          .required(true)
          .takes_value(true)
          .help("Lender's name.")))
      .arg(Arg::with_name("id")
        .short("i")
        .long("id")
        .takes_value(true)
        .help("Lender id."))
      .subcommand(SubCommand::with_name("view_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .takes_value(true)
          .help("Display the loan with the specified id only."))
        .arg(Arg::with_name("filter")
          .short("f")
          .long("filter")
          .takes_value(true)
          .possible_values(&["requested", "fulfilled", "declined", "active", "complete"])
          .help("Display the loan with the specified status only."))
        .help("By default, display all loans of this lender."))
      .subcommand(SubCommand::with_name("fulfill_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .required(true)
          .takes_value(true)
          .help("Loan id."))
        .arg(Arg::with_name("issuer")
          .short("i")
          .long("issuer")
          .required(true)
          .takes_value(true)
          .help("Asset issuer id."))
        .arg(Arg::with_name("memo_file")
          .short("f")
          .long("memo_file")
          .takes_value(true)
          .help("If specified, will store the asset tracer memo and owner memo."))
        .arg(Arg::with_name("http")
          .long("http")
          .takes_value(false)
          .help("Specify that http, not https should be used."))
        .arg(Arg::with_name("localhost")
          .long("localhost")
          .takes_value(false)
          .help("Specify that localhost, not testnet.findora.org should be used.")))
      .subcommand(SubCommand::with_name("create_or_overwrite_requirement")
        .arg(Arg::with_name("attribute")
          .short("a")
          .long("attribute")
          .required(true)
          .takes_value(true)
          .possible_values(&["min_credit_score", "min_income", "citizenship"])
          .help("Credential attribute."))
        .arg(Arg::with_name("requirement")
          .short("r")
          .long("requirement")
          .required(true)
          .takes_value(true)
          .help("Required value of the credential record."))
        .help("Create or overwrite a credential requirement.")))
    .subcommand(SubCommand::with_name("borrower")
      .subcommand(SubCommand::with_name("sign_up")
        .arg(Arg::with_name("name")
          .short("n")
          .long("name")
          .required(true)
          .takes_value(true)
          .help("Borrower's name.")))
      .arg(Arg::with_name("id")
        .short("i")
        .long("id")
        .takes_value(true)
        .help("Borrower id."))
      .subcommand(SubCommand::with_name("load_funds")
        .arg(Arg::with_name("issuer")
          .short("i")
          .long("issuer")
          .takes_value(true)
          .help("Required: issuer id."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount to transfer to the recipient."))
        .arg(Arg::with_name("http")
          .long("http")
          .takes_value(false)
          .help("Specify that http, not https should be used."))
        .arg(Arg::with_name("localhost")
          .long("localhost")
          .takes_value(false)
          .help("Specify that localhost, not testnet.findora.org should be used.")))
      .subcommand(SubCommand::with_name("view_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .takes_value(true)
          .help("Display the loan with the specified id only."))
        .arg(Arg::with_name("filter")
          .short("f")
          .long("filter")
          .takes_value(true)
          .possible_values(&["requested", "fulfilled", "declined", "active", "complete"])
          .help("Display the loan with the specified status only."))
        .help("By default, display all loans of this borrower."))
      .subcommand(SubCommand::with_name("request_loan")
        .arg(Arg::with_name("lender")
          .short("l")
          .long("lender")
          .required(true)
          .takes_value(true)
          .help("Lender id."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount of the loan."))
        .arg(Arg::with_name("interest_per_mille")
          .short("i")
          .long("interest_per_mille")
          .required(true)
          .takes_value(true)
          .help("Interest per mille. The interest rate will be interest_per_mille/1000."))
        .arg(Arg::with_name("duration")
          .short("d")
          .long("duration")
          .required(true)
          .takes_value(true)
          .help("Payment duration")))
      .subcommand(SubCommand::with_name("pay_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .required(true)
          .takes_value(true)
          .help("Loan id."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Payment amount."))
        .arg(Arg::with_name("http")
          .long("http")
          .takes_value(false)
          .help("Specify that http, not https should be used."))
        .arg(Arg::with_name("localhost")
          .long("localhost")
          .takes_value(false)
          .help("Specify that localhost, not testnet.findora.org should be used.")))
      .subcommand(SubCommand::with_name("view_credential")
        .arg(Arg::with_name("attribute")
          .short("a")
          .long("attribute")
          .takes_value(true)
          .possible_values(&["min_credit_score", "min_income", "citizenship"])
          .help("Display the specified credential attribute only."))
        .help("By default, display all credentials of this borrower."))
      .subcommand(SubCommand::with_name("create_or_overwrite_credential")
        .arg(Arg::with_name("credential_issuer")
          .short("c")
          .long("credential_issuer")
          .required(true)
          .takes_value(true)
          .help("Credential issuer id."))
        .arg(Arg::with_name("attribute")
          .short("a")
          .long("attribute")
          .required(true)
          .takes_value(true)
          .possible_values(&["min_credit_score", "min_income", "citizenship"])
          .help("Credential attribute."))
        .arg(Arg::with_name("value")
          .short("v")
          .long("value")
          .required(true)
          .takes_value(true)
          .help("Value of the credential record."))
        .help("Create or overwrite a credential record."))
      .subcommand(SubCommand::with_name("get_asset_record")
        .arg(Arg::with_name("sid")
          .long("sid")
          .short("s")
          .takes_value(true)
          .help("Asset sid."))
        .arg(Arg::with_name("memo_file")
          .short("f")
          .long("memo_file")
          .takes_value(true)
          .help("Path to the tracer and owner memos."))
        .arg(Arg::with_name("http")
          .long("http")
          .takes_value(false)
          .help("Specify that http, not https should be used."))
        .arg(Arg::with_name("localhost")
          .long("localhost")
          .takes_value(false)
          .help("Specify that localhost, not testnet.findora.org should be used."))))
    .subcommand(SubCommand::with_name("create_txn_builder")
      .about("By default, will rename previous file with a .<number> suffix")
      .arg(Arg::with_name("name")
        .short("n")
        .long("name")
        .value_name("FILE")
        .help("Specify a name for newly created transaction file")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("If specified, the existing file with the same name will be overwritten.")))
    .subcommand(SubCommand::with_name("serialize"))
    .subcommand(SubCommand::with_name("submit")
      .arg(Arg::with_name("get_sids")
        .long("get_sids")
        .short("g")
        .takes_value(false)
        .help("If specified, will query the utxo sids."))
      .arg(Arg::with_name("sids_file")
        .long("sids_file")
        .short("s")
        .takes_value(true)
        .help("If specified, will store the utxo sids to the file."))
      .arg(Arg::with_name("http")
        .long("http")
        .takes_value(false)
        .help("Specify that http, not https should be used."))
      .arg(Arg::with_name("localhost")
        .long("localhost")
        .takes_value(false)
        .help("Specify that localhost, not testnet.findora.org should be used.")))
}
