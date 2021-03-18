#![deny(warnings)]

mod shared;

use air::{check_merkle_proof, AIRResult};
use credentials::{credential_verify, CredCommitment};
use ruc::*;
use shared::{AIRAddressAndPoK, PubCreds, RevealFields};
use utils::{protocol_host, urlencode, LEDGER_PORT};

#[tokio::main]
async fn main() -> Result<()> {
    // Step 1: Retrieve the address and pok from user
    env_logger::init();
    let credname = urlencode("passport");
    let issuer_resp =
        attohttpc::get(&format!("http://localhost:3030/issuer_pk/{}", &credname))
            .send()
            .c(d!())?
            .json::<PubCreds>()
            .c(d!())?;
    println!("Response from issuer is:\n{:?}", &issuer_resp);

    let attr_map = vec![
        (String::from("sex"), &b"M"[..]),
        (String::from("pob"), &b"666"[..]),
    ];
    let reveal_fields = RevealFields {
        fields: attr_map.clone().into_iter().map(|(f, _)| f).collect(),
    };
    let reveal_fields_str = urlencode(&serde_json::to_string(&reveal_fields).c(d!())?);
    let req_string = format!("http://127.0.0.1:3031/reveal/{}/", &credname);

    println!(
        "req string is:\n{}, urlencoded reveal_fields string is {}",
        &req_string, reveal_fields_str
    );

    let addr_and_pok: AIRAddressAndPoK = attohttpc::post(&req_string)
        .json::<RevealFields>(&reveal_fields)
        .c(d!())?
        .send()
        .c(d!())?
        .json::<AIRAddressAndPoK>()
        .c(d!())?;

    println!(
        "Response from user is: addr ={}",
        serde_json::to_string(&addr_and_pok.addr).unwrap()
    );

    // Step 2: Get value and Merkle proof from AIR address

    let (protocol, host) = protocol_host();
    println!(
        "Looking up value at {}://{}:{}/air_address/{}",
        protocol,
        host,
        LEDGER_PORT,
        serde_json::to_string(&addr_and_pok.addr).unwrap()
    );
    let air_result: AIRResult = attohttpc::get(&format!(
        "{}://{}:{}/air_address/{}",
        protocol, host, LEDGER_PORT, &addr_and_pok.addr
    ))
    .send()
    .c(d!())?
    .json::<AIRResult>()
    .c(d!())?;

    println!("Response from ledger is:\n{:?}", &air_result);

    let air_entry = air_result.value.clone();

    if let Some(commitment_string) = air_entry {
        let commitment: CredCommitment =
            serde_json::from_str(&commitment_string[..]).unwrap();
        if check_merkle_proof(
            &air_result.merkle_root,
            &air_result.key,
            air_result.value.as_ref(),
            &air_result.merkle_proof,
        ) {
            credential_verify(
                &issuer_resp.issuer_pk,
                &attr_map[..],
                &commitment,
                &addr_and_pok.pok,
            )
            .c(d!("smt merkle proof succeeds but ac_verify fails"))
        } else {
            Err(eg!("AIR merkle proof failed"))
        }
    } else {
        Err(eg!(format!("No AIR entry at {}", &addr_and_pok.addr)))
    }
}
