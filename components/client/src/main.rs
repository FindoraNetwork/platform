// use json::{parse, stringify_pretty};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let resp = reqwest::get("https://testnet.findora.org:8668/block_log")
        .await?
        .text()
        .await?;
    // let json_struct = parse(&resp)?;
    println!("{:#?}", resp); //stringify_pretty(json_struct, 2));
    Ok(())
}