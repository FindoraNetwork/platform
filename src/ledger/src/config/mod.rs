//! Configuration of checkpoints.
use std::{fs::File, io::{Read, ErrorKind}};
use std::io::Write;
use serde::{Deserialize, Serialize};
use toml;

#[derive(Serialize, Deserialize, Default)]
#[allow(missing_docs)]
pub struct CheckPointConfig {
    pub disable_evm_block_height: i64,
    pub enable_frc20_height: i64,
    pub evm_first_block_height: i64,
    pub zero_amount_fix_height: u64,
    pub apy_fix_height: u64,
    pub overflow_fix_height: u64,
    pub second_fix_height: u64,
    pub apy_v7_upgrade_height: u64,
    pub ff_addr_extra_fix_height: u64,
    pub nonconfidential_balance_fix_height: u64,
}

impl CheckPointConfig {
    /// load configuration of checkpoints from file.
    pub fn from_file() -> Option<CheckPointConfig> {
        let file_path = "checkpoint.toml".to_string();
        let mut f = match File::open(&file_path) {
            Ok(file) => file,
            Err(error) => {
                if error.kind() == ErrorKind::NotFound {
                    match File::create(&file_path) {
                        Ok(mut file) => {
                            let config = CheckPointConfig{
                                disable_evm_block_height: 1483286,
                                enable_frc20_height: 1501000,
                                evm_first_block_height: 0,
                                zero_amount_fix_height: 1200000,
                                apy_fix_height: 1177000,
                                overflow_fix_height: 1247000,
                                second_fix_height: 1429000,
                                apy_v7_upgrade_height: 1429000,
                                ff_addr_extra_fix_height: 1200000,
                                nonconfidential_balance_fix_height: 1210000,
                            };
                            let content = toml::to_string(&config).unwrap();
                            file.write_all(content.as_bytes()).unwrap();
                            return Some(config)
                        }
                        Err(error) => {
                            panic!("failed to create file: {:?}, file_path: {:?}", error, file_path)
                        }
                    };
                } else {
                    panic!("failed to open file: {:?}", error)
                }
            }
        };

        let mut content = String::new();
        f.read_to_string(&mut content).unwrap();
        let config: CheckPointConfig = toml::from_str(content.as_str()).unwrap();
        Some(config)
    }
}