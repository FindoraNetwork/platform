use clap::{App, Arg, SubCommand};
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::AssetTypeCode;
use ledger::{des_fail, error_location};
use ledger_api_service::RestfulLedgerAccess;
use network::{HttpStandaloneConfig, LedgerStandalone};
use serde::{Deserialize, Serialize};
use solvency::*;
use std::fs;
use zei::errors::ZeiError;

/// Path to the data file.
const DATA_FILE: &str = "solvency_data.json";

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Information of assets, liabilities and conversion rates.
struct AssetLiabilityAndRateData {
    /// Asset and liability account
    asset_and_liability_account: AssetAndLiabilityAccount,

    /// Contains the asset conversion rate
    solvency_audit: SolvencyAudit,
}

/// Stores a string to the specified file.
fn store_string_to_file(string_data: &str, file: &str) -> Result<(), PlatformError> {
    match fs::write(file, string_data) {
        Ok(_) => Ok(()),
        Err(e) => Err(PlatformError::IoError(format!(
            "Failed to create file {}: {}.",
            file, e
        ))),
    }
}

/// Stores the program data to `DATA_FILE`, when the program starts or the data is updated.
fn store_data_to_file(
    data_dir: &str,
    data: AssetLiabilityAndRateData,
) -> Result<(), PlatformError> {
    let data_file_path = format!("{}/{}", data_dir, DATA_FILE);
    if let Ok(data_string) = serde_json::to_string(&data) {
        store_string_to_file(&data_string, &data_file_path)?;
    } else {
    }
    Ok(())
}

/// Loads data.
/// * If the data file exists, loads data from it.
/// * Otherwise, stores the initial data to file and returns the data.
fn load_data(data_dir: &str) -> Result<AssetLiabilityAndRateData, PlatformError> {
    let data_file_path = format!("{}/{}", data_dir, DATA_FILE);
    let data = match fs::read_to_string(data_file_path) {
        Ok(data) => data,
        Err(_) => {
            let init_data = AssetLiabilityAndRateData::default();
            store_data_to_file(data_dir, init_data.clone())?;
            return Ok(init_data);
        }
    };

    serde_json::from_str::<AssetLiabilityAndRateData>(&data).map_err(|e| des_fail!(e))
}

/// Parses a string to u64.
fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
    val_str
        .trim()
        .parse::<u64>()
        .map_err(|_| PlatformError::InputsError(error_location!()))
}

/// Processes input commands and arguments.
fn process_inputs<T: RestfulLedgerAccess>(
    inputs: clap::ArgMatches,
    rest_client: &T,
) -> Result<(), PlatformError> {
    let dir = if let Some(d) = inputs.value_of("dir") {
        d
    } else {
        println!("Missing directory to store data. Use --dir.");
        return Err(PlatformError::InputsError(error_location!()));
    };
    let mut data = load_data(dir)?;
    match inputs.subcommand() {
        ("set_rate", Some(set_matches)) => {
            let code = if let Some(code_arg) = set_matches.value_of("code") {
                AssetTypeCode::new_from_base64(code_arg)?
            } else {
                println!("Missing asset code. Use --code.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let rate = if let Some(rate_arg) = set_matches.value_of("rate") {
                parse_to_u64(rate_arg)?
            } else {
                println!("Missing conversion rate. Use --rate.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            data.solvency_audit.set_rate(code, rate);
            store_data_to_file(dir, data)
        }
        ("add_asset_or_liability", Some(add_matches)) => {
            let amount_type = if let Some(type_arg) = add_matches.value_of("type") {
                match type_arg {
                    "asset" => AmountType::Asset,
                    _ => AmountType::Liability,
                }
            } else {
                println!("Missing asset or liability type. Use --type.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let amount = if let Some(amount_arg) = add_matches.value_of("amount") {
                parse_to_u64(amount_arg)?
            } else {
                println!("Missing amount of the asset or liability. Use --amount.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let code = if let Some(code_arg) = add_matches.value_of("code") {
                AssetTypeCode::new_from_base64(code_arg)?
            } else {
                println!("Missing asset code. Use --code.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            let blinds = if let Some(blinds_arg) = add_matches.value_of("blinds") {
                Some(
                    serde_json::from_str::<((Scalar, Scalar), Scalar)>(&blinds_arg)
                        .map_err(|e| des_fail!(e))?,
                )
            } else {
                None
            };
            let utxo = if let Some(utxo_arg) = add_matches.value_of("utxo") {
                parse_to_u64(utxo_arg)?
            } else {
                println!("Missing UTXO of the asset or liability. Use --utxo.");
                return Err(PlatformError::InputsError(error_location!()));
            };
            data.asset_and_liability_account.update(
                amount_type,
                amount,
                code,
                blinds,
                utxo,
                rest_client,
            )?;
            store_data_to_file(dir, data)
        }
        ("prove_and_verify_solvency", Some(prove_and_verify_matches)) => {
            let mut hidden_assets = if let Some(hidden_assets_arg) =
                prove_and_verify_matches.value_of("hidden_assets")
            {
                serde_json::from_str::<Vec<AmountAndCodeScalar>>(&hidden_assets_arg)
                    .map_err(|e| des_fail!(e))?
            } else {
                Vec::new()
            };
            let mut hidden_assets_blinds = if let Some(hidden_assets_blinds_arg) =
                prove_and_verify_matches.value_of("hidden_assets_blinds")
            {
                serde_json::from_str::<Vec<AmountAndCodeBlinds>>(
                    &hidden_assets_blinds_arg,
                )
                .map_err(|e| des_fail!(e))?
            } else {
                Vec::new()
            };
            let mut hidden_liabilities = if let Some(hidden_liabilities_arg) =
                prove_and_verify_matches.value_of("hidden_liabilities")
            {
                serde_json::from_str::<Vec<AmountAndCodeScalar>>(&hidden_liabilities_arg)
                    .map_err(|e| des_fail!(e))?
            } else {
                Vec::new()
            };
            let mut hidden_liabilities_blinds =
                if let Some(hidden_liabilities_blinds_arg) =
                    prove_and_verify_matches.value_of("hidden_liabilities_blinds")
                {
                    serde_json::from_str::<Vec<AmountAndCodeBlinds>>(
                        &hidden_liabilities_blinds_arg,
                    )
                    .map_err(|e| des_fail!(e))?
                } else {
                    Vec::new()
                };
            data.solvency_audit.prove_solvency_and_store(
                &mut data.asset_and_liability_account,
                &mut hidden_assets,
                &mut hidden_assets_blinds,
                &mut hidden_liabilities,
                &mut hidden_liabilities_blinds,
            )?;
            match data
                .solvency_audit
                .verify_solvency(&data.asset_and_liability_account)
            {
                Ok(_) => {
                    println!("Solvency proof and verification succeeded.");
                }
                _ => {
                    println!("Solvency proof and verification failed.");
                    return Err(PlatformError::ZeiError(
                        error_location!(),
                        ZeiError::SolvencyVerificationError,
                    ));
                }
            }
            store_data_to_file(dir, data)
        }
        _ => {
            println!("Subcommand missing or not recognized. Try --help");
            Err(PlatformError::InputsError(error_location!()))
        }
    }
}

fn get_cli_app<'a, 'b>() -> App<'a, 'b> {
    App::new("Solvency Proof")
    .version(concat!("0.0.1 Build: ",env!("VERGEN_SHA_SHORT")," ", env!("VERGEN_BUILD_DATE")))
    .about("Copyright 2020 © Findora. All rights reserved.")
    .arg(Arg::with_name("dir")
      .short("d")
      .long("dir")
      .value_name("PATH")
      .required(true)
      .takes_value(true)
      .help("Directory to store data"))
    .arg(Arg::with_name("local")
      .long("local")
      .help("If local flag is specified, transactions will be submitted to a local ledger"))
    .subcommand(SubCommand::with_name("set_rate")
      .arg(Arg::with_name("code")
        .short("c")
        .long("code")
        .required(true)
        .takes_value(true)
        .help("Asset code."))
      .arg(Arg::with_name("rate")
        .short("r")
        .long("rate")
        .required(true)
        .takes_value(true)
        .help("Conversion rate of this asset.")))
    .subcommand(SubCommand::with_name("add_asset_or_liability")
      .arg(Arg::with_name("type")
        .short("t")
        .long("type")
        .required(true)
        .takes_value(true)
        .possible_values(&["asset", "liability"])
        .help("Specify whether to add asset or liability."))
      .arg(Arg::with_name("amount")
        .short("a")
        .long("amount")
        .required(true)
        .takes_value(true)
        .help("Amount of the asset or liability."))
      .arg(Arg::with_name("code")
        .short("c")
        .long("code")
        .required(true)
        .takes_value(true)
        .help("Code of the asset or liability."))
      .arg(Arg::with_name("blinds")
        .short("b")
        .long("blinds")
        .required(false)
        .takes_value(true)
        .help("Serialized ((low asset amount blind, high assset amount blind), asset code blind) for confidential amount."))
      .arg(Arg::with_name("utxo")
        .short("u")
        .long("utxo")
        .required(true)
        .takes_value(true)
        .help("UTXO of the asset or liability.")))
    .subcommand(SubCommand::with_name("prove_and_verify_solvency")
      .arg(Arg::with_name("hidden_assets")
        .short("a")
        .long("hidden_assets")
        .takes_value(true)
        .help("Serialized asset amounts and codes in Scalar."))
      .arg(Arg::with_name("hidden_assets_blinds")
        .short("s")
        .long("hidden_assets_blinds")
        .takes_value(true)
        .help("Serialized blinding values of asset amounts and codes."))
      .arg(Arg::with_name("hidden_liabilities")
        .short("l")
        .long("hidden_liabilities")
        .takes_value(true)
        .help("Serialized liability amounts and codes in Scalar."))
      .arg(Arg::with_name("hidden_liabilities_blinds")
        .short("i")
        .long("hidden_liabilities_blinds")
        .takes_value(true)
        .help("Serialized blinding values of liability amounts and codes.")))
}

fn main() -> Result<(), PlatformError> {
    let app = get_cli_app();
    let inputs = app.get_matches();
    let local = inputs.value_of("local").is_some();
    let config = {
        if local {
            HttpStandaloneConfig::local()
        } else {
            HttpStandaloneConfig::testnet()
        }
    };

    let rest_client = LedgerStandalone::new_http(&config);
    process_inputs(inputs, &rest_client)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ledger::data_model::AssetRules;
    use network::MockLedgerStandalone;
    use rand_chacha::ChaChaRng;
    use rand_core::{CryptoRng, RngCore, SeedableRng};
    use tempfile::tempdir;
    use txn_cli::txn_lib::{define_and_submit, issue_transfer_and_get_utxo_and_blinds};
    use zei::xfr::asset_record::AssetRecordType;
    use zei::xfr::sig::XfrKeyPair;

    fn submit_command(
        cmd_vec: Vec<&str>,
        rest_client: &mut MockLedgerStandalone,
    ) -> Result<(), PlatformError> {
        let app = get_cli_app();
        let inputs = app.get_matches_from_safe(cmd_vec).unwrap();
        process_inputs(inputs, rest_client)
    }

    // Command to set asset conversion rates
    fn set_rate_cmd(
        dir: &str,
        code: &str,
        rate: &str,
        rest_client: &mut MockLedgerStandalone,
    ) -> Result<(), PlatformError> {
        submit_command(
            vec![
                "Solvency Proof",
                "--dir",
                dir,
                "set_rate",
                "--code",
                code,
                "--rate",
                rate,
            ],
            rest_client,
        )
    }

    // Issue and transfer assets, and get the serialized UTXOs and blinds
    fn issue_transfer_multiple<R: CryptoRng + RngCore>(
        issuer_key_pair: &XfrKeyPair,
        recipient_key_pair: &XfrKeyPair,
        codes: Vec<AssetTypeCode>,
        prng: &mut R,
        ledger_standalone: &mut MockLedgerStandalone,
    ) -> (Vec<String>, Vec<String>) {
        let mut utxos = Vec::new();
        let mut blinds = Vec::new();
        let (utxo_0, _, _) = issue_transfer_and_get_utxo_and_blinds(
            issuer_key_pair,
            recipient_key_pair,
            10,
            codes[0],
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            0,
            prng,
            ledger_standalone,
        )
        .unwrap();
        utxos.push(format!("{}", utxo_0));
        let (utxo_1, amount_blinds_1, code_blind_1) =
            issue_transfer_and_get_utxo_and_blinds(
                issuer_key_pair,
                recipient_key_pair,
                200,
                codes[1],
                AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                1,
                prng,
                ledger_standalone,
            )
            .unwrap();
        utxos.push(format!("{}", utxo_1));
        blinds.push(serde_json::to_string(&(amount_blinds_1, code_blind_1)).unwrap());
        let (utxo_2, amount_blinds_2, code_blind_2) =
            issue_transfer_and_get_utxo_and_blinds(
                issuer_key_pair,
                recipient_key_pair,
                3,
                codes[2],
                AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                2,
                prng,
                ledger_standalone,
            )
            .unwrap();
        utxos.push(format!("{}", utxo_2));
        blinds.push(serde_json::to_string(&(amount_blinds_2, code_blind_2)).unwrap());
        let (utxo_3, _, _) = issue_transfer_and_get_utxo_and_blinds(
            issuer_key_pair,
            recipient_key_pair,
            40,
            codes[0],
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            3,
            prng,
            ledger_standalone,
        )
        .unwrap();
        utxos.push(format!("{}", utxo_3));
        let (utxo_4, amount_blinds_4, code_blind_4) =
            issue_transfer_and_get_utxo_and_blinds(
                issuer_key_pair,
                recipient_key_pair,
                50,
                codes[1],
                AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                4,
                prng,
                ledger_standalone,
            )
            .unwrap();
        utxos.push(format!("{}", utxo_4));
        blinds.push(serde_json::to_string(&(amount_blinds_4, code_blind_4)).unwrap());
        let (utxo_5, amount_blinds_5, code_blind_5) =
            issue_transfer_and_get_utxo_and_blinds(
                issuer_key_pair,
                recipient_key_pair,
                150,
                codes[1],
                AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
                5,
                prng,
                ledger_standalone,
            )
            .unwrap();
        utxos.push(format!("{}", utxo_5));
        blinds.push(serde_json::to_string(&(amount_blinds_5, code_blind_5)).unwrap());
        let (utxo_6, _, _) = issue_transfer_and_get_utxo_and_blinds(
            issuer_key_pair,
            recipient_key_pair,
            30,
            codes[0],
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            6,
            prng,
            ledger_standalone,
        )
        .unwrap();
        utxos.push(format!("{}", utxo_6));
        (utxos, blinds)
    }

    // Command to add a confidential asset or liability
    fn add_confidential_asset_or_liability_cmd(
        dir: &str,
        amount_type: &str,
        amount: &str,
        code: &str,
        blinds: &str,
        utxo: &str,
        rest_client: &MockLedgerStandalone,
    ) -> Result<(), PlatformError> {
        let app = get_cli_app();
        let inputs = app.get_matches_from(vec![
            "Solvency Proof",
            "--dir",
            dir,
            "add_asset_or_liability",
            "--type",
            amount_type,
            "--amount",
            amount,
            "--code",
            code,
            "--blinds",
            blinds,
            "--utxo",
            utxo,
        ]);
        process_inputs(inputs, rest_client)
    }

    // Command to add a nonconfidential asset or liability
    fn add_nonconfidential_asset_or_liability_cmd(
        dir: &str,
        amount_type: &str,
        amount: &str,
        code: &str,
        utxo: &str,
        rest_client: &MockLedgerStandalone,
    ) -> Result<(), PlatformError> {
        let app = get_cli_app();
        let inputs = app.get_matches_from(vec![
            "Solvency Proof",
            "--dir",
            dir,
            "add_asset_or_liability",
            "--type",
            amount_type,
            "--amount",
            amount,
            "--code",
            code,
            "--utxo",
            utxo,
        ]);
        process_inputs(inputs, rest_client)
    }

    // Command to prove and verify solvency
    fn prove_and_verify_solvency_cmd(
        dir: &str,
        hidden_assets: Vec<AmountAndCodeScalar>,
        hidden_assets_blinds: Vec<AmountAndCodeBlinds>,
        hidden_liabilities: Vec<AmountAndCodeScalar>,
        hidden_liabilities_blinds: Vec<AmountAndCodeBlinds>,
        rest_client: &mut MockLedgerStandalone,
    ) -> Result<(), PlatformError> {
        let hidden_assets_str = serde_json::to_string(&hidden_assets).unwrap();
        let hidden_assets_blinds_str =
            serde_json::to_string(&hidden_assets_blinds).unwrap();
        let hidden_liabilities_str = serde_json::to_string(&hidden_liabilities).unwrap();
        let hidden_liabilities_blinds_str =
            serde_json::to_string(&hidden_liabilities_blinds).unwrap();

        let args = vec![
            "Solvency Proof",
            "--dir",
            dir,
            "prove_and_verify_solvency",
            "--hidden_assets",
            &hidden_assets_str,
            "--hidden_assets_blinds",
            &hidden_assets_blinds_str,
            "--hidden_liabilities",
            &hidden_liabilities_str,
            "--hidden_liabilities_blinds",
            &hidden_liabilities_blinds_str,
        ];

        submit_command(args, rest_client)
    }

    #[cfg(test)]
    #[allow(unused)]
    // This test fails a clean build on master
    // See https://bugtracker.findora.org/issues/130
    fn test_cmd() {
        let tmp_dir = tempdir().unwrap();
        let dir = tmp_dir.path().to_str().unwrap();

        // Start the standalone ledger
        let mut ledger_standalone = MockLedgerStandalone::new_mock(1);

        // Generate asset codes and key pairs
        let codes = vec![
            AssetTypeCode::gen_random(),
            AssetTypeCode::gen_random(),
            AssetTypeCode::gen_random(),
        ];
        let issuer_key_pair = &XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
        let recipient_key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

        // Define, issue and transfer assets
        for code in codes.iter() {
            define_and_submit(
                &issuer_key_pair,
                *code,
                AssetRules::default(),
                &mut ledger_standalone,
            )
            .unwrap();
        }
        let code_0 = &codes[0].to_base64();
        let code_1 = &codes[1].to_base64();
        let code_2 = &codes[2].to_base64();
        let (utxos, blinds) = issue_transfer_multiple(
            issuer_key_pair,
            &recipient_key_pair,
            codes.clone(),
            &mut ChaChaRng::from_entropy(),
            &mut ledger_standalone,
        );

        // Set asset conversion rates
        set_rate_cmd(dir, code_0, "1", &mut ledger_standalone)
            .expect("Failed to set conversion rate.");
        set_rate_cmd(dir, code_1, "100", &mut ledger_standalone)
            .expect("Failed to set conversion rate.");
        set_rate_cmd(dir, code_2, "1", &mut ledger_standalone)
            .expect("Failed to set conversion rate.");

        // Add assets and liabilities such that total asset amount > total liabiliity amount
        let hidden_assets: &mut Vec<AmountAndCodeScalar> = &mut Vec::new();
        let hidden_assets_blinds: &mut Vec<AmountAndCodeBlinds> = &mut Vec::new();
        let hidden_liabilities: &mut Vec<AmountAndCodeScalar> = &mut Vec::new();
        let hidden_liabilities_blinds: &mut Vec<AmountAndCodeBlinds> = &mut Vec::new();

        add_nonconfidential_asset_or_liability_cmd(
            dir,
            "asset",
            "10",
            code_0,
            &utxos[0],
            &mut ledger_standalone,
        )
        .expect("Failed to add public asset.");

        add_confidential_asset_or_liability_cmd(
            dir,
            "asset",
            "200",
            code_1,
            &blinds[0],
            &utxos[1],
            &mut ledger_standalone,
        )
        .expect("Failed to add hidden asset.");

        add_confidential_asset_or_liability_cmd(
            dir,
            "asset",
            "3",
            code_2,
            &blinds[1],
            &utxos[2],
            &mut ledger_standalone,
        )
        .expect("Failed to add hidden asset.");

        add_nonconfidential_asset_or_liability_cmd(
            dir,
            "liability",
            "40",
            code_0,
            &utxos[3],
            &mut ledger_standalone,
        )
        .expect("Failed to add public liability.");

        add_confidential_asset_or_liability_cmd(
            dir,
            "liability",
            "50",
            code_1,
            &blinds[2],
            &utxos[4],
            &mut ledger_standalone,
        )
        .expect("Failed to add hidden liability.");

        // Prove and verify solvency
        hidden_assets.push(get_amount_and_code_scalars(200, codes[1]));
        hidden_assets_blinds.push(calculate_amount_and_code_blinds(&blinds[0]).unwrap());
        hidden_assets.push(get_amount_and_code_scalars(3, codes[2]));
        hidden_assets_blinds.push(calculate_amount_and_code_blinds(&blinds[1]).unwrap());
        hidden_liabilities.push(get_amount_and_code_scalars(50, codes[1]));
        hidden_liabilities_blinds
            .push(calculate_amount_and_code_blinds(&blinds[2]).unwrap());
        prove_and_verify_solvency_cmd(
            dir,
            hidden_assets.to_vec(),
            hidden_assets_blinds.to_vec(),
            hidden_liabilities.to_vec(),
            hidden_liabilities_blinds.to_vec(),
            &mut ledger_standalone,
        )
        .expect("Failed to prove and verify solvency.");

        // Add additional liabilities to make total asset amount < total liabiliity amount
        add_confidential_asset_or_liability_cmd(
            dir,
            "liability",
            "150",
            code_1,
            &blinds[3],
            &utxos[5],
            &mut ledger_standalone,
        )
        .expect("Failed to add hidden liability.");
        // Prove and verify solvency
        // Should fail since total asset amount < total liabiliity amount
        hidden_liabilities.push(get_amount_and_code_scalars(150, codes[1]));
        hidden_liabilities_blinds
            .push(calculate_amount_and_code_blinds(&blinds[3]).unwrap());
        let res = prove_and_verify_solvency_cmd(
            dir,
            hidden_assets.to_vec(),
            hidden_assets_blinds.to_vec(),
            hidden_liabilities.to_vec(),
            hidden_liabilities_blinds.to_vec(),
            &mut ledger_standalone,
        );

        assert!(res.is_err());
        add_nonconfidential_asset_or_liability_cmd(
            dir,
            "asset",
            "30",
            code_0,
            &utxos[6],
            &mut ledger_standalone,
        )
        .expect("Failed to add public asset.");

        // Prove and verify solvency
        prove_and_verify_solvency_cmd(
            dir,
            hidden_assets.to_vec(),
            hidden_assets_blinds.to_vec(),
            hidden_liabilities.to_vec(),
            hidden_liabilities_blinds.to_vec(),
            &mut ledger_standalone,
        )
        .expect("Failed to prove and verify solvency.");

        tmp_dir.close().unwrap();
    }
}
