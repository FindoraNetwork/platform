use ledger::data_model::{Operation, Transaction, AssetTokenCode};
use ledger::store::helpers::*;
// use ledger::store::{ArchiveUpdate, LedgerState, LedgerUpdate};
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use rand::SeedableRng;
use rand_chacha::ChaChaRng;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut tx = Transaction::default();

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    // env_logger::init();

    let serialize = serde_json::to_string(&tx).unwrap();
    // Set of invalid URI characters that may appear in a JSON transaction
    const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ')
                                         .add(b'"')
                                         .add(b'`')
                                         .add(b'{')
                                         .add(b'/')
                                         .add(b'}');
    let uri_string = utf8_percent_encode(&serialize, FRAGMENT).to_string();
 
    let client = reqwest::Client::new();

    let token_code_b64 = token_code1.to_base64();
    println!("\n\nQuery asset_token {:?}", &token_code1);

    let mut res = reqwest::get(&format!("http://localhost:8668/asset_token/{}", &token_code_b64))?;

    println!("Status: {}", res.status());
    println!("Headers:\n{:?}", res.headers());

    // copy the response body directly to stdout
    std::io::copy(&mut res, &mut std::io::stdout())?;

    println!("\n\nSubmit transaction: uri_string=\"{:?}\"", &uri_string);

    res = client.post(&format!("http://localhost:8668/submit_transaction/{}", uri_string)).body("").send()?;
    println!("Status: {}", res.status());
    println!("Headers:\n{:?}", res.headers());

    // copy the response body directly to stdout
    std::io::copy(&mut res, &mut std::io::stdout())?;

    println!("\n\nQuery global_state {:?} again", &token_code1);
    res = reqwest::get(&format!("http://localhost:8668/global_state/{}", 0))?;

    println!("Status: {}", res.status());
    println!("Headers:\n{:?}", res.headers());

    // copy the response body directly to stdout
    std::io::copy(&mut res, &mut std::io::stdout())?;

    println!("\n\nDone.");

    Ok(())
}
