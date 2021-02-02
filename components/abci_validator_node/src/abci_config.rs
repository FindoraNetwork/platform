use serde_derive::Deserialize;
use std::path::Path;
use toml;

#[derive(Deserialize)]
pub struct ABCIConfig {
    pub abci_host: String,
    pub abci_port: String,
    pub tendermint_host: String,
    pub tendermint_port: String,
    pub submission_host: String,
    pub submission_port: String,
    pub ledger_host: String,
    pub ledger_port: String,
}

impl Default for ABCIConfig {
    fn default() -> ABCIConfig {
        // tendermint -------> abci(host, port)
        let abci_host = std::env::var_os("ABCI_HOST")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| "127.0.0.1".into());
        let abci_port = std::env::var_os("ABCI_PORT")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| "26658".into());

        // abci ----> tendermint(host, port)
        let tendermint_port = std::env::var_os("TENDERMINT_PORT")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| "26657".into());
        let tendermint_host = std::env::var_os("TENDERMINT_HOST")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| "localhost".into());

        // client ------> abci(host, port, for submission)
        let submission_host = std::env::var_os("SERVER_HOST")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| "localhost".into());
        let submission_port = std::env::var_os("SUBMISSION_PORT")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| "8669".into());

        // client ------> abci(host, port, for ledger access)
        let ledger_host = submission_host.clone();
        let ledger_port = std::env::var_os("LEDGER_PORT")
            .filter(|x| !x.is_empty())
            .and_then(|x| x.into_string().ok())
            .unwrap_or_else(|| "8668".into());

        ABCIConfig {
            abci_host,
            abci_port,
            tendermint_host,
            tendermint_port,
            submission_host,
            submission_port,
            ledger_host,
            ledger_port,
        }
    }
}

impl ABCIConfig {
    pub fn from_file(args: &mut Vec<String>) -> (ABCIConfig, bool) {
        // ensure that 'abci' dir exists
        let abci_path = Path::new(&args[1]).join("abci");
        assert!(abci_path.is_dir(), "directory abci not found");
        args[1] = abci_path.as_path().to_str().unwrap().to_owned();

        // use abci.toml if it exists
        let abci_toml = abci_path.join("abci.toml");
        if abci_toml.is_file() {
            let contents = std::fs::read_to_string(abci_toml.to_str().unwrap()).unwrap();
            let config: ABCIConfig = toml::from_str(&contents).unwrap();
            return (config, true);
        } else {
            return (Default::default(), false);
        }
    }
}
