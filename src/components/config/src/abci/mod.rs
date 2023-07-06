use {
    global_cfg::CFG,
    lazy_static::lazy_static,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::{
        convert::TryFrom,
        env,
        fs::{self, File},
        io::{ErrorKind, Read, Write},
        path::Path,
    },
    toml,
};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[allow(missing_docs)]
pub struct CheckPointConfig {
    // https://github.com/FindoraNetwork/platform/pull/211
    // Enable evm substate.
    pub evm_substate_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/178
    // Disable evm.
    pub disable_evm_block_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/181
    // fix node crash and reset update height.
    pub enable_frc20_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/191
    pub tx_revert_on_error_height: i64,

    // Commit: d449b7c97850b225cf26c72c8b19ed284d6d7101
    pub evm_first_block_height: i64,

    // Commit: a1cfa708074df18379e0cf01d4df48794c5d100d
    // Fix a BUG in the calculation of commission.
    pub zero_amount_fix_height: u64,

    // Commit: 69ca4865842a3a1eef628a5ceab5e856c3e866c4,
    // Rename: a1cfa708074df18379e0cf01d4df48794c5d100d
    pub apy_fix_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/98
    // Fix delegation rewards overflow using bigint.
    pub overflow_fix_height: u64,

    // Commit c9d2b4f5760cb5bd79848b451fca56c023b1cc71
    // Sync APY v7 upgrade block height.
    pub second_fix_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/97
    // Change APY modifier based on v7 rate.
    pub apy_v7_upgrade_height: u64,

    // Commit: a1cfa708074df18379e0cf01d4df48794c5d100d
    // Add an extra `reserved` address; fix a BUG in the calculation of commission.
    pub ff_addr_extra_fix_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/93
    // Fix incorrect calculations about nonconfidential balances.
    pub nonconfidential_balance_fix_height: u64,

    pub unbond_block_cnt: u64,

    // https://github.com/FindoraNetwork/platform/pull/307
    // Fix unpaid delegation.
    pub fix_unpaid_delegation_height: u64,

    // https://github.com/FindoraNetwork/platform/pull/430
    // Fix missing rewards within 21 days after undelegation.
    pub fix_undelegation_missing_reward_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/316
    // FO-968: Increment Nonce for CheckTx run mode without executing EVM transaction PORT TO MAIN.
    pub evm_checktx_nonce: i64,

    // https://github.com/FindoraNetwork/platform/pull/345
    // Fix the problem of utxo transaction body without signature.
    pub utxo_checktx_height: i64,

    // https://github.com/FindoraNetwork/platform/pull/434
    // Fix the amount in the delegators that staking did not modify when it punished the validator.
    pub fix_delegators_am_height: u64,
    pub validators_limit_v2_height: u64,

    // Enable evm substate v2
    pub evm_substate_v2_height: i64,

    pub disable_delegate_frc20: i64,

    pub fix_exec_code: i64,

    #[serde(default = "def_check_signatures_num")]
    pub check_signatures_num: i64,

    // https://github.com/FindoraNetwork/platform/pull/707
    // FO-1370: V0.3.30 EVM bug: receipt missing when error code === 1
    #[serde(default = "def_fix_deliver_tx_revert_nonce_height")]
    pub fix_deliver_tx_revert_nonce_height: i64,

    #[serde(default = "def_utxo_asset_prefix_height")]
    pub utxo_asset_prefix_height: u64,

    #[serde(default = "def_prismxx_inital_height")]
    pub prismxx_inital_height: i64,

    #[serde(default = "def_prism_bridge_address")]
    pub prism_bridge_address: String,

    #[serde(default = "def_remove_fake_staking_hash")]
    pub remove_fake_staking_hash: u64,

    #[serde(default = "def_fix_check_replay")]
    pub fix_check_replay: u64,

    #[serde(default = "def_fns_registry")]
    pub fns_registry: String,

    #[serde(default = "def_lowlevel_data_min")]
    pub lowlevel_data_min: u64,

    #[serde(default = "def_lowlevel_data_max")]
    pub lowlevel_data_max: u64,

    #[serde(default = "def_validator_whitelist_v1_height")]
    pub validator_whitelist_v1_height: u64,

    #[serde(default = "def_validator_whitelist_v1")]
    pub validator_whitelist_v1: Vec<String>,

    #[serde(default = "def_validator_whitelist_v2_height")]
    pub validator_whitelist_v2_height: u64,

    #[serde(default = "def_validator_whitelist_v2")]
    pub validator_whitelist_v2: Vec<String>,

    #[serde(default = "def_validator_whitelist_v3_height")]
    pub validator_whitelist_v3_height: u64,

    #[serde(default = "def_validator_whitelist_v3")]
    pub validator_whitelist_v3: Vec<String>,

    #[serde(default = "def_max_gas_price_limit")]
    pub max_gas_price_limit: i64,

    #[serde(default = "def_evm_staking_inital_height")]
    pub evm_staking_inital_height: i64,

    #[serde(default = "def_evm_staking_address")]
    pub evm_staking_address: String,
}

fn def_fix_check_replay() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.fix_check_replay
}

fn def_check_signatures_num() -> i64 {
    DEFAULT_CHECKPOINT_CONFIG.check_signatures_num
}

fn def_fix_deliver_tx_revert_nonce_height() -> i64 {
    DEFAULT_CHECKPOINT_CONFIG.fix_deliver_tx_revert_nonce_height
}

fn def_utxo_asset_prefix_height() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.utxo_asset_prefix_height
}

fn def_prismxx_inital_height() -> i64 {
    DEFAULT_CHECKPOINT_CONFIG.prismxx_inital_height
}

fn def_prism_bridge_address() -> String {
    DEFAULT_CHECKPOINT_CONFIG.prism_bridge_address.clone()
}

fn def_remove_fake_staking_hash() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.remove_fake_staking_hash
}

fn def_fns_registry() -> String {
    DEFAULT_CHECKPOINT_CONFIG.fns_registry.clone()
}

fn def_lowlevel_data_min() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.lowlevel_data_min
}

fn def_lowlevel_data_max() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.lowlevel_data_max
}

fn def_validator_whitelist_v1_height() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.validator_whitelist_v1_height
}

fn def_validator_whitelist_v1() -> Vec<String> {
    DEFAULT_CHECKPOINT_CONFIG.validator_whitelist_v1.clone()
}

fn def_validator_whitelist_v2_height() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.validator_whitelist_v2_height
}

fn def_validator_whitelist_v2() -> Vec<String> {
    DEFAULT_CHECKPOINT_CONFIG.validator_whitelist_v2.clone()
}
fn def_validator_whitelist_v3_height() -> u64 {
    DEFAULT_CHECKPOINT_CONFIG.validator_whitelist_v3_height
}

fn def_validator_whitelist_v3() -> Vec<String> {
    DEFAULT_CHECKPOINT_CONFIG.validator_whitelist_v3.clone()
}

fn def_max_gas_price_limit() -> i64 {
    DEFAULT_CHECKPOINT_CONFIG.max_gas_price_limit
}

fn def_evm_staking_inital_height() -> i64 {
    DEFAULT_CHECKPOINT_CONFIG.evm_staking_inital_height
}

fn def_evm_staking_address() -> String {
    DEFAULT_CHECKPOINT_CONFIG.evm_staking_address.clone()
}

#[cfg(feature = "debug_env")]
lazy_static! {
    static ref DEFAULT_CHECKPOINT_CONFIG: CheckPointConfig = CheckPointConfig {
        evm_substate_height: 0,
        disable_evm_block_height: 0,
        enable_frc20_height: 0,
        tx_revert_on_error_height: 0,
        evm_first_block_height: 0,
        zero_amount_fix_height: 0,
        apy_fix_height: 0,
        overflow_fix_height: 0,
        second_fix_height: 0,
        apy_v7_upgrade_height: 0,
        ff_addr_extra_fix_height: 0,
        nonconfidential_balance_fix_height: 0,
        unbond_block_cnt: 3600 * 24 * 21 / 16,
        fix_unpaid_delegation_height: 0,
        fix_undelegation_missing_reward_height: 0,
        evm_checktx_nonce: 0,
        utxo_checktx_height: 0,
        fix_delegators_am_height: 0,
        validators_limit_v2_height: 0,
        evm_substate_v2_height: 0,
        disable_delegate_frc20: 0,
        fix_exec_code: 0,
        check_signatures_num: 0,
        fix_deliver_tx_revert_nonce_height: 0,
        utxo_asset_prefix_height: 0,
        prismxx_inital_height: 128,
        prism_bridge_address: "0x5f9552fEd754F20B636C996DaDB32806554Bb995".to_owned(),
        remove_fake_staking_hash: 0,
        fix_check_replay: 0,
        fns_registry: "".to_owned(),
        lowlevel_data_min: 0,
        lowlevel_data_max: 0,
        validator_whitelist_v1_height: 0,
        validator_whitelist_v1: vec![],
        validator_whitelist_v2_height: 0,
        validator_whitelist_v2: vec![],
        validator_whitelist_v3_height: 0,
        validator_whitelist_v3: vec![],
        max_gas_price_limit: 0,
        evm_staking_inital_height: 128,
        evm_staking_address: "0x84db796A3F8F02396f82219e3197933d15960771".to_owned(),
    };
}

#[cfg(not(feature = "debug_env"))]
lazy_static! {
    static ref DEFAULT_CHECKPOINT_CONFIG: CheckPointConfig = CheckPointConfig {
        evm_substate_height: 1802500,
        disable_evm_block_height: 1483286,
        enable_frc20_height: 1501000,
        tx_revert_on_error_height: 1624077,
        evm_first_block_height: 1424654,
        zero_amount_fix_height: 1200000,
        apy_fix_height: 1177000,
        overflow_fix_height: 1247000,
        second_fix_height: 1429000,
        apy_v7_upgrade_height: 1429000,
        ff_addr_extra_fix_height: 1200000,
        nonconfidential_balance_fix_height: 1210000,
        unbond_block_cnt: 3600 * 24 * 21 / 16,
        fix_unpaid_delegation_height: 2261885,
        fix_undelegation_missing_reward_height: 3351349,
        evm_checktx_nonce: 3351349,
        utxo_checktx_height: 3351349,
        fix_delegators_am_height: 3351349,
        validators_limit_v2_height: 3351349,
        evm_substate_v2_height: 3351349,
        disable_delegate_frc20: 3401450,
        fix_exec_code: 3401450,
        check_signatures_num: 4004430,
        fix_deliver_tx_revert_nonce_height: 4004430,
        utxo_asset_prefix_height: 4004430,
        prismxx_inital_height: 4004430,
        prism_bridge_address: "0x4672372fDB139B7295Fc59b55b43EC5fF2761A0b".to_owned(),
        remove_fake_staking_hash: 4004430,
        fix_check_replay: 4004430,
        fns_registry: "0x57e8782c2f77B99823EeA48aCE3Eb7635F0B35F9".to_owned(),
        lowlevel_data_min: 3971239,
        lowlevel_data_max: 4004430,
        validator_whitelist_v1_height: 4072340,
        validator_whitelist_v1: vec![
            "2A75D9238DBBF14891F7BFFBBA7EF86CA0E98CC9".to_string(),
            "09EF1DB6B67D1CBF7EBA6BD9B204611848993DF7".to_string(),
            "E012AA66C83999E3862C8AA534B9CE66FC14A37A".to_string(),
            "544FEC0D957816C880F1AC4C4CA239FEEDE0AC70".to_string(),
            "D7A838C7A7F2526AADCCBCB95348F8F68404693C".to_string(),
            "61ED9D4018B10E9B007D200725CCA0087544268F".to_string(),
            "805B1F87212164FD1DB64B8ED63A8F2C42AAC647".to_string(),
            "5C97EE9B91D90B332813078957E3A96B304791B4".to_string(),
            "0856654F7CD4BB0D6CC4409EF4892136C9D24692".to_string(),
            "A8DFD116BA9664F38958C721688FA73E6320755B".to_string(),
            "54937E208CF724F06CA723173C54FC5E8F9AD01A".to_string(),
            "000E33AB7471186F3B1DE9FC08BB9C480F453590".to_string(),
            "FD8C65634A9D8899FA14200177AF19D24F6E1C37".to_string(),
            "E8F6748439DA597A43ED150F55F6B48E30494BD6".to_string(),
            "B4989BBB38287C2AF6DF0155B55E4073DA6C4BA8".to_string(),
            "1D8F397FA03B357DC94303086A91CE5C8C7AF1E6".to_string(),
            "916AD122B85C16BEE71723E52F727EB5A705123F".to_string(),
            "E5705FED0049EDA431D37B37947A136F22F8F054".to_string(),
            "EA70EB6087E3D606730C4E9062CC24A5BD7D2B37".to_string(),
            "236960CC4506F7A051FDF0DAC07F2AE9B9AAE63C".to_string(),
            "1DE3EED26BB6CBAE7C6F5A8B881EF36F78F72AAF".to_string(),
            "9AB077E00C8B731AE1F82DEC5E45CB3D1E9BBB12".to_string(),
            "60689516C566F27E03794329C431D0084299480A".to_string(),
            "7956EFAAFC81CF155207E33FCED084C326A5972D".to_string(),
            "5AEBB0871B9EEAE510CB10C45E09FED03FB233D8".to_string(),
            "69E2B6C4C1122172E69AF48E0AEC36B7F7C8005A".to_string(),
            "8A2D072955AA021425379894377949494FACF072".to_string(),
            "55DBB6B98E70F4A9905C880B7C66282B5D5AD000".to_string(),
            "9ED0D8D661C99A58F78F80816968E61AAE8DC649".to_string(),
            "26AA7581263332F47E0CE17CF4B1F34D22C7F4CB".to_string(),
            "9F7223691393DA99A68E962A20B8578ED03A7158".to_string(),
            "8CB713C8EA32223FCAC66B966FCFA9BAEE257946".to_string(),
            "7C77CF71CF6CBD04885E32FC49EDA367F7BC3C65".to_string(),
            "629F2D3DA692107BFC5DB3122C44FCFAA72DB8C7".to_string(),
            "EAC5792572EB726AA0DBA9A7AFA9757F8063C6C9".to_string(),
            "510082967DFA7DEBA11267B26A6318D07A457B48".to_string(),
            "AD2C69A9432E8F6634E1ADC3D6CA69EA9E1F4114".to_string(),
            "39F0C5E451394FAAE7213FD914EFBA8F963CCB90".to_string(),
            "47480785029886997002807DEFD0A7E3FF37CF90".to_string(),
            "3C59A3A7B114DD22F8DB48A5941D974C93099524".to_string(),
            "107A17BF72756F6539A1C65A788F896665021C6A".to_string(),
            "4CF02F4E4861D01D5DABB49661B5A874B634E16E".to_string(),
            "F4986612979B95716AA8ABADA15E9A9DB725D691".to_string(),
            "E6DB58F174A0B96C8D3C662B87562A1781B3C3A9".to_string(),
            "168E7692F3D6F36E124D9883217F610DA807EAD0".to_string(),
            "3560FD0632B4E2F4F16490BBD9CD0A763045BF35".to_string(),
            "A50D65F2F63F65D845A7C5CBB989FF94D6688F38".to_string(),
            "00121F5CFD8D95F8C194ED4CCFF47BBD1904B791".to_string(),
            "A07875BBD4E062BAB2C162E180237FC3B30C4ABC".to_string(),
            "F4E3DDE487B4AD132A802E14E8E93A4A3ABA1C42".to_string(),
            "B54F747973A17B6D47264077090A347B65CDD472".to_string(),
            "F6D51E19E146E8CCFCCDB65164E66E3773BA6936".to_string(),
            "4E3DA3856567E4AB21B70C25FB7C19729FCEEBCA".to_string(),
            "E657C713BCD25F960B676E2E824C9E05DEFDB7A2".to_string(),
            "874D5BE395E223D136C3AB7047CB0ABA255047E4".to_string(),
            "5C71532CEEFC43EE3857905AB94FDA505BFC06F3".to_string(),
            "7E8CF431861A6C2C2D2DB6D05362CC01283B3883".to_string(),
            "251E1C0B0DE110386282EE77ED09CD5920BE211F".to_string(),
            "BD0607CB96F22781FAD19E47BE4CB12D782938DE".to_string(),
            "CDA403F892C5597FC5DAA263900108CA802017FE".to_string(),
            "2440346158429CEAE65C15121D0C40560820CFC2".to_string(),
            "DDAF8255C863B296E1D8EBE20D85F66870669FF5".to_string(),
            "B846EB4DF4B1BE5EFA824172873B62453BFF272D".to_string(),
            "D709796D52923734403B8A5F3F02A06F7142AA26".to_string(),
            "EE2F73BAA1605C998BB106E5A38DBD79B5209F1D".to_string(),
            "3DEE751B9661025B602D7D0C1B95C341D5CF6233".to_string(),
            "3E108EA444087E0D804A39A665AEB0D319E94BC0".to_string(),
            "5517FBE15F292D4900EA9E7B9E01FF80709B6DC8".to_string(),
            "5F14A9FAE42C014C452F2E3AE9DF005C4551459D".to_string(),
            "C071B7470168B36F82160428F8FE715360D2E9FD".to_string(),
            "B9ECD265A9D6116F93CD24B39282346A7BE86FD7".to_string(),
            "F5F8E98165DC92810041D6BDC543DB71BC74FE6C".to_string(),
            "BA6B2FC297E4B609A270FA8BBCEA9747E2DD9B98".to_string(),
            "88152D67E67DA0E8605E61736EAC06356134E0B3".to_string(),
            "8118E9D209F67FF4FC840FF183BED5E4CAE76E11".to_string(),
            "7EFE6655436794BE8720D0B0EFDFFDC2A8BFF9E4".to_string(),
            "80E064A00A421569F1222ECAB1E296D7F58D8354".to_string(),
            "CCAC2728809428D8D2967B3D9093D6A989DF072A".to_string(),
            "E39CC9F3F7A3551DBD63714E6CB198EC434E6609".to_string(),
            "0DDFF0C4B29A2DE638C1C29C65817546C64883C0".to_string(),
            "400C0F623F71C8BFEB6B4EC71B54624925C1A6C6".to_string(),
            "B83C70895668466787EFD9351486CE18F1220CCB".to_string(),
            "9E6717392EFDCFA101E33449A7C2A238251315B1".to_string(),
            "47034DC213A160EA8B0B4B5605F70B08004F4F3B".to_string(),
            "3752E29778C960E0BD18DD6EB1FCE60FF6787F1A".to_string(),
            "5542CE9C23809CB025A3CA8428DDCB1FE2CED8D6".to_string(),
            "CF676FC9E754D46295AF50865145A7C6E13F2B68".to_string(),
            "9832263E4644EF7B1B58B357714B9AC7C3BBDCA5".to_string(),
            "60E656A26F316A6687E8BB2716E43B077CAC1AEA".to_string(),
            "0786901B984EF28A065D7345155D662E51FF42F3".to_string(),
            "BA66D94100E1181868FA6137F7EF612F379B35F6".to_string(),
            "497A27354A8ACF326AB8C0FF281A1B4A86FCFD6F".to_string(),
            "D123F4524224CF32937BAB0CF8F40DD2C3C417C4".to_string(),
        ],
        validator_whitelist_v2_height: 4111000,
        validator_whitelist_v2: vec![
            "2A75D9238DBBF14891F7BFFBBA7EF86CA0E98CC9".to_string(),
            "09EF1DB6B67D1CBF7EBA6BD9B204611848993DF7".to_string(),
            "E012AA66C83999E3862C8AA534B9CE66FC14A37A".to_string(),
            "544FEC0D957816C880F1AC4C4CA239FEEDE0AC70".to_string(),
            "D7A838C7A7F2526AADCCBCB95348F8F68404693C".to_string(),
            "61ED9D4018B10E9B007D200725CCA0087544268F".to_string(),
            "805B1F87212164FD1DB64B8ED63A8F2C42AAC647".to_string(),
            "5C97EE9B91D90B332813078957E3A96B304791B4".to_string(),
            "0856654F7CD4BB0D6CC4409EF4892136C9D24692".to_string(),
            "A8DFD116BA9664F38958C721688FA73E6320755B".to_string(),
            "54937E208CF724F06CA723173C54FC5E8F9AD01A".to_string(),
            "000E33AB7471186F3B1DE9FC08BB9C480F453590".to_string(),
            "FD8C65634A9D8899FA14200177AF19D24F6E1C37".to_string(),
            "E8F6748439DA597A43ED150F55F6B48E30494BD6".to_string(),
            "B4989BBB38287C2AF6DF0155B55E4073DA6C4BA8".to_string(),
            "1D8F397FA03B357DC94303086A91CE5C8C7AF1E6".to_string(),
            "916AD122B85C16BEE71723E52F727EB5A705123F".to_string(),
            "E5705FED0049EDA431D37B37947A136F22F8F054".to_string(),
            "EA70EB6087E3D606730C4E9062CC24A5BD7D2B37".to_string(),
            "236960CC4506F7A051FDF0DAC07F2AE9B9AAE63C".to_string(),
            "1DE3EED26BB6CBAE7C6F5A8B881EF36F78F72AAF".to_string(),
            "9AB077E00C8B731AE1F82DEC5E45CB3D1E9BBB12".to_string(),
            "60689516C566F27E03794329C431D0084299480A".to_string(),
            "7956EFAAFC81CF155207E33FCED084C326A5972D".to_string(),
            "5AEBB0871B9EEAE510CB10C45E09FED03FB233D8".to_string(),
            "69E2B6C4C1122172E69AF48E0AEC36B7F7C8005A".to_string(),
            "8A2D072955AA021425379894377949494FACF072".to_string(),
            "55DBB6B98E70F4A9905C880B7C66282B5D5AD000".to_string(),
            "9ED0D8D661C99A58F78F80816968E61AAE8DC649".to_string(),
            "26AA7581263332F47E0CE17CF4B1F34D22C7F4CB".to_string(),
            "9F7223691393DA99A68E962A20B8578ED03A7158".to_string(),
            "8CB713C8EA32223FCAC66B966FCFA9BAEE257946".to_string(),
            "7C77CF71CF6CBD04885E32FC49EDA367F7BC3C65".to_string(),
            "629F2D3DA692107BFC5DB3122C44FCFAA72DB8C7".to_string(),
            "EAC5792572EB726AA0DBA9A7AFA9757F8063C6C9".to_string(),
            "510082967DFA7DEBA11267B26A6318D07A457B48".to_string(),
            "AD2C69A9432E8F6634E1ADC3D6CA69EA9E1F4114".to_string(),
            "39F0C5E451394FAAE7213FD914EFBA8F963CCB90".to_string(),
            "47480785029886997002807DEFD0A7E3FF37CF90".to_string(),
            "3C59A3A7B114DD22F8DB48A5941D974C93099524".to_string(),
            "107A17BF72756F6539A1C65A788F896665021C6A".to_string(),
            "4CF02F4E4861D01D5DABB49661B5A874B634E16E".to_string(),
            "F4986612979B95716AA8ABADA15E9A9DB725D691".to_string(),
            "E6DB58F174A0B96C8D3C662B87562A1781B3C3A9".to_string(),
            "168E7692F3D6F36E124D9883217F610DA807EAD0".to_string(),
            "3560FD0632B4E2F4F16490BBD9CD0A763045BF35".to_string(),
            "A50D65F2F63F65D845A7C5CBB989FF94D6688F38".to_string(),
            "00121F5CFD8D95F8C194ED4CCFF47BBD1904B791".to_string(),
            "A07875BBD4E062BAB2C162E180237FC3B30C4ABC".to_string(),
            "F4E3DDE487B4AD132A802E14E8E93A4A3ABA1C42".to_string(),
            "B54F747973A17B6D47264077090A347B65CDD472".to_string(),
            "F6D51E19E146E8CCFCCDB65164E66E3773BA6936".to_string(),
            "4E3DA3856567E4AB21B70C25FB7C19729FCEEBCA".to_string(),
            "E657C713BCD25F960B676E2E824C9E05DEFDB7A2".to_string(),
            "874D5BE395E223D136C3AB7047CB0ABA255047E4".to_string(),
            "5C71532CEEFC43EE3857905AB94FDA505BFC06F3".to_string(),
            "7E8CF431861A6C2C2D2DB6D05362CC01283B3883".to_string(),
            "251E1C0B0DE110386282EE77ED09CD5920BE211F".to_string(),
            "BD0607CB96F22781FAD19E47BE4CB12D782938DE".to_string(),
            "CDA403F892C5597FC5DAA263900108CA802017FE".to_string(),
            "2440346158429CEAE65C15121D0C40560820CFC2".to_string(),
            "DDAF8255C863B296E1D8EBE20D85F66870669FF5".to_string(),
            "B846EB4DF4B1BE5EFA824172873B62453BFF272D".to_string(),
            "D709796D52923734403B8A5F3F02A06F7142AA26".to_string(),
            "EE2F73BAA1605C998BB106E5A38DBD79B5209F1D".to_string(),
            "3DEE751B9661025B602D7D0C1B95C341D5CF6233".to_string(),
            "3E108EA444087E0D804A39A665AEB0D319E94BC0".to_string(),
            "5517FBE15F292D4900EA9E7B9E01FF80709B6DC8".to_string(),
            "5F14A9FAE42C014C452F2E3AE9DF005C4551459D".to_string(),
            "C071B7470168B36F82160428F8FE715360D2E9FD".to_string(),
            "B9ECD265A9D6116F93CD24B39282346A7BE86FD7".to_string(),
            "F5F8E98165DC92810041D6BDC543DB71BC74FE6C".to_string(),
            "BA6B2FC297E4B609A270FA8BBCEA9747E2DD9B98".to_string(),
            "88152D67E67DA0E8605E61736EAC06356134E0B3".to_string(),
            "8118E9D209F67FF4FC840FF183BED5E4CAE76E11".to_string(),
            "7EFE6655436794BE8720D0B0EFDFFDC2A8BFF9E4".to_string(),
            "80E064A00A421569F1222ECAB1E296D7F58D8354".to_string(),
            "CCAC2728809428D8D2967B3D9093D6A989DF072A".to_string(),
            "E39CC9F3F7A3551DBD63714E6CB198EC434E6609".to_string(),
            "0DDFF0C4B29A2DE638C1C29C65817546C64883C0".to_string(),
            "400C0F623F71C8BFEB6B4EC71B54624925C1A6C6".to_string(),
            "B83C70895668466787EFD9351486CE18F1220CCB".to_string(),
            "9E6717392EFDCFA101E33449A7C2A238251315B1".to_string(),
            "47034DC213A160EA8B0B4B5605F70B08004F4F3B".to_string(),
            "3752E29778C960E0BD18DD6EB1FCE60FF6787F1A".to_string(),
            "5542CE9C23809CB025A3CA8428DDCB1FE2CED8D6".to_string(),
            "CF676FC9E754D46295AF50865145A7C6E13F2B68".to_string(),
            "9832263E4644EF7B1B58B357714B9AC7C3BBDCA5".to_string(),
            "60E656A26F316A6687E8BB2716E43B077CAC1AEA".to_string(),
            "0786901B984EF28A065D7345155D662E51FF42F3".to_string(),
            "BA66D94100E1181868FA6137F7EF612F379B35F6".to_string(),
            "497A27354A8ACF326AB8C0FF281A1B4A86FCFD6F".to_string(),
            "D123F4524224CF32937BAB0CF8F40DD2C3C417C4".to_string(),
            "D518C4F95A3F39ED853A2614566897C4AD5A008F".to_string(),
            "BD1292DBAD51A5DBD60685040816B450F9C6C4A4".to_string(),
        ],
        validator_whitelist_v3_height: 4188600,
        validator_whitelist_v3: vec![
            "2A75D9238DBBF14891F7BFFBBA7EF86CA0E98CC9".to_string(),
            "09EF1DB6B67D1CBF7EBA6BD9B204611848993DF7".to_string(),
            "E012AA66C83999E3862C8AA534B9CE66FC14A37A".to_string(),
            "544FEC0D957816C880F1AC4C4CA239FEEDE0AC70".to_string(),
            "D7A838C7A7F2526AADCCBCB95348F8F68404693C".to_string(),
            "61ED9D4018B10E9B007D200725CCA0087544268F".to_string(),
            "805B1F87212164FD1DB64B8ED63A8F2C42AAC647".to_string(),
            "5C97EE9B91D90B332813078957E3A96B304791B4".to_string(),
            "0856654F7CD4BB0D6CC4409EF4892136C9D24692".to_string(),
            "A8DFD116BA9664F38958C721688FA73E6320755B".to_string(),
            "54937E208CF724F06CA723173C54FC5E8F9AD01A".to_string(),
            "000E33AB7471186F3B1DE9FC08BB9C480F453590".to_string(),
            "FD8C65634A9D8899FA14200177AF19D24F6E1C37".to_string(),
            "E8F6748439DA597A43ED150F55F6B48E30494BD6".to_string(),
            "B4989BBB38287C2AF6DF0155B55E4073DA6C4BA8".to_string(),
            "1D8F397FA03B357DC94303086A91CE5C8C7AF1E6".to_string(),
            "916AD122B85C16BEE71723E52F727EB5A705123F".to_string(),
            "E5705FED0049EDA431D37B37947A136F22F8F054".to_string(),
            "EA70EB6087E3D606730C4E9062CC24A5BD7D2B37".to_string(),
            "236960CC4506F7A051FDF0DAC07F2AE9B9AAE63C".to_string(),
            "1DE3EED26BB6CBAE7C6F5A8B881EF36F78F72AAF".to_string(),
            "9AB077E00C8B731AE1F82DEC5E45CB3D1E9BBB12".to_string(),
            "60689516C566F27E03794329C431D0084299480A".to_string(),
            "7956EFAAFC81CF155207E33FCED084C326A5972D".to_string(),
            "5AEBB0871B9EEAE510CB10C45E09FED03FB233D8".to_string(),
            "69E2B6C4C1122172E69AF48E0AEC36B7F7C8005A".to_string(),
            "8A2D072955AA021425379894377949494FACF072".to_string(),
            "55DBB6B98E70F4A9905C880B7C66282B5D5AD000".to_string(),
            "9ED0D8D661C99A58F78F80816968E61AAE8DC649".to_string(),
            "26AA7581263332F47E0CE17CF4B1F34D22C7F4CB".to_string(),
            "9F7223691393DA99A68E962A20B8578ED03A7158".to_string(),
            "8CB713C8EA32223FCAC66B966FCFA9BAEE257946".to_string(),
            "7C77CF71CF6CBD04885E32FC49EDA367F7BC3C65".to_string(),
            "629F2D3DA692107BFC5DB3122C44FCFAA72DB8C7".to_string(),
            "EAC5792572EB726AA0DBA9A7AFA9757F8063C6C9".to_string(),
            "510082967DFA7DEBA11267B26A6318D07A457B48".to_string(),
            "AD2C69A9432E8F6634E1ADC3D6CA69EA9E1F4114".to_string(),
            "39F0C5E451394FAAE7213FD914EFBA8F963CCB90".to_string(),
            "47480785029886997002807DEFD0A7E3FF37CF90".to_string(),
            "3C59A3A7B114DD22F8DB48A5941D974C93099524".to_string(),
            "107A17BF72756F6539A1C65A788F896665021C6A".to_string(),
            "4CF02F4E4861D01D5DABB49661B5A874B634E16E".to_string(),
            "F4986612979B95716AA8ABADA15E9A9DB725D691".to_string(),
            "E6DB58F174A0B96C8D3C662B87562A1781B3C3A9".to_string(),
            "168E7692F3D6F36E124D9883217F610DA807EAD0".to_string(),
            "3560FD0632B4E2F4F16490BBD9CD0A763045BF35".to_string(),
            "A50D65F2F63F65D845A7C5CBB989FF94D6688F38".to_string(),
            "00121F5CFD8D95F8C194ED4CCFF47BBD1904B791".to_string(),
            "A07875BBD4E062BAB2C162E180237FC3B30C4ABC".to_string(),
            "F4E3DDE487B4AD132A802E14E8E93A4A3ABA1C42".to_string(),
            "B54F747973A17B6D47264077090A347B65CDD472".to_string(),
            "F6D51E19E146E8CCFCCDB65164E66E3773BA6936".to_string(),
            "4E3DA3856567E4AB21B70C25FB7C19729FCEEBCA".to_string(),
            "E657C713BCD25F960B676E2E824C9E05DEFDB7A2".to_string(),
            "874D5BE395E223D136C3AB7047CB0ABA255047E4".to_string(),
            "5C71532CEEFC43EE3857905AB94FDA505BFC06F3".to_string(),
            "7E8CF431861A6C2C2D2DB6D05362CC01283B3883".to_string(),
            "251E1C0B0DE110386282EE77ED09CD5920BE211F".to_string(),
            "BD0607CB96F22781FAD19E47BE4CB12D782938DE".to_string(),
            "CDA403F892C5597FC5DAA263900108CA802017FE".to_string(),
            "2440346158429CEAE65C15121D0C40560820CFC2".to_string(),
            "DDAF8255C863B296E1D8EBE20D85F66870669FF5".to_string(),
            "B846EB4DF4B1BE5EFA824172873B62453BFF272D".to_string(),
            "D709796D52923734403B8A5F3F02A06F7142AA26".to_string(),
            "EE2F73BAA1605C998BB106E5A38DBD79B5209F1D".to_string(),
            "3DEE751B9661025B602D7D0C1B95C341D5CF6233".to_string(),
            "3E108EA444087E0D804A39A665AEB0D319E94BC0".to_string(),
            "5517FBE15F292D4900EA9E7B9E01FF80709B6DC8".to_string(),
            "5F14A9FAE42C014C452F2E3AE9DF005C4551459D".to_string(),
            "C071B7470168B36F82160428F8FE715360D2E9FD".to_string(),
            "B9ECD265A9D6116F93CD24B39282346A7BE86FD7".to_string(),
            "F5F8E98165DC92810041D6BDC543DB71BC74FE6C".to_string(),
            "BA6B2FC297E4B609A270FA8BBCEA9747E2DD9B98".to_string(),
            "88152D67E67DA0E8605E61736EAC06356134E0B3".to_string(),
            "8118E9D209F67FF4FC840FF183BED5E4CAE76E11".to_string(),
            "7EFE6655436794BE8720D0B0EFDFFDC2A8BFF9E4".to_string(),
            "80E064A00A421569F1222ECAB1E296D7F58D8354".to_string(),
            "CCAC2728809428D8D2967B3D9093D6A989DF072A".to_string(),
            "E39CC9F3F7A3551DBD63714E6CB198EC434E6609".to_string(),
            "0DDFF0C4B29A2DE638C1C29C65817546C64883C0".to_string(),
            "400C0F623F71C8BFEB6B4EC71B54624925C1A6C6".to_string(),
            "B83C70895668466787EFD9351486CE18F1220CCB".to_string(),
            "9E6717392EFDCFA101E33449A7C2A238251315B1".to_string(),
            "47034DC213A160EA8B0B4B5605F70B08004F4F3B".to_string(),
            "3752E29778C960E0BD18DD6EB1FCE60FF6787F1A".to_string(),
            "5542CE9C23809CB025A3CA8428DDCB1FE2CED8D6".to_string(),
            "CF676FC9E754D46295AF50865145A7C6E13F2B68".to_string(),
            "9832263E4644EF7B1B58B357714B9AC7C3BBDCA5".to_string(),
            "60E656A26F316A6687E8BB2716E43B077CAC1AEA".to_string(),
            "0786901B984EF28A065D7345155D662E51FF42F3".to_string(),
            "BA66D94100E1181868FA6137F7EF612F379B35F6".to_string(),
            "497A27354A8ACF326AB8C0FF281A1B4A86FCFD6F".to_string(),
            "D123F4524224CF32937BAB0CF8F40DD2C3C417C4".to_string(),
            "D518C4F95A3F39ED853A2614566897C4AD5A008F".to_string(),
            "37D3228A650F591522698BECDF42DCE5D1113D88".to_string(),
            "577F8548D8F834D39D26350D2A3A928F478AF5FD".to_string(),
        ],
        max_gas_price_limit: 5000000,
        evm_staking_inital_height: 5000000,
        evm_staking_address: "".to_owned(),
    };
}

impl CheckPointConfig {
    /// load configuration of checkpoints from file.
    pub fn from_file(file_path: &str) -> Option<CheckPointConfig> {
        let mut f = match File::open(file_path) {
            Ok(file) => file,
            Err(error) => {
                if error.kind() == ErrorKind::NotFound {
                    match File::create(file_path) {
                        Ok(mut file) => {
                            let config = (*DEFAULT_CHECKPOINT_CONFIG).clone();
                            let content = toml::to_string(&config).unwrap();
                            file.write_all(content.as_bytes()).unwrap();
                            return Some(config);
                        }
                        Err(error) => {
                            panic!("failed to create file: {error:?}",)
                        }
                    };
                } else {
                    panic!("failed to open file: {error:?}",)
                }
            }
        };

        let mut content = String::new();
        f.read_to_string(&mut content).unwrap();
        let config: CheckPointConfig = toml::from_str(content.as_str())
            .or_else(|_| serde_json::from_str(content.as_str()))
            .unwrap();
        Some(config)
    }
}

#[derive(Debug)]
pub struct ABCIConfig {
    pub abci_host: String,
    pub abci_port: u16,
    pub tendermint_host: String,
    pub tendermint_port: u16,
    pub submission_port: u16,
    pub ledger_port: u16,
    pub query_port: u16,
    pub evm_http_port: u16,
    pub evm_ws_port: u16,
    pub ledger_dir: String,
}

#[derive(Deserialize)]
pub struct ABCIConfigStr {
    pub abci_host: String,
    pub abci_port: String,
    pub tendermint_host: String,
    pub tendermint_port: String,
    pub submission_port: String,
    pub ledger_port: String,
    pub evm_http_port: String,
    pub evm_ws_port: String,
    #[serde(skip)]
    pub ledger_dir: Option<String>,
}

impl TryFrom<ABCIConfigStr> for ABCIConfig {
    type Error = Box<dyn RucError>;
    fn try_from(cfg: ABCIConfigStr) -> Result<Self> {
        let ledger_port = cfg.ledger_port.parse::<u16>().c(d!())?;
        let query_port = ledger_port - 1;
        let evm_http_port = cfg.evm_http_port.parse::<u16>().c(d!())?;
        let evm_ws_port = cfg.evm_ws_port.parse::<u16>().c(d!())?;
        Ok(ABCIConfig {
            abci_host: cfg.abci_host,
            abci_port: cfg.abci_port.parse::<u16>().c(d!())?,
            tendermint_host: cfg.tendermint_host,
            tendermint_port: cfg.tendermint_port.parse::<u16>().c(d!())?,
            submission_port: cfg.submission_port.parse::<u16>().c(d!())?,
            ledger_port,
            query_port,
            evm_http_port,
            evm_ws_port,
            ledger_dir: cfg
                .ledger_dir
                .unwrap_or_else(|| pnk!(env::var("LEDGER_DIR"))),
        })
    }
}

impl ABCIConfig {
    pub fn from_env() -> Result<ABCIConfig> {
        let abci_host = CFG.abci_host.to_owned();
        let abci_port = CFG.abci_port;

        let tendermint_host = CFG.tendermint_host.to_owned();
        let tendermint_port = CFG.tendermint_port;

        // client ------> abci(host, port, for submission)
        let submission_port = CFG.submission_service_port;

        // client ------> abci(host, port, for ledger access)
        let ledger_port = CFG.ledger_service_port;

        let query_port = ledger_port - 1;

        Ok(ABCIConfig {
            abci_host,
            abci_port,
            tendermint_host,
            tendermint_port,
            submission_port,
            ledger_port,
            query_port,
            evm_http_port: CFG.evm_http_port,
            evm_ws_port: CFG.evm_ws_port,
            ledger_dir: CFG.ledger_dir.clone(),
        })
    }

    pub fn from_file() -> Result<ABCIConfig> {
        let config_path = Path::new(&CFG.ledger_dir).join("abci.toml");
        let file_contents = fs::read_to_string(config_path).c(d!())?;
        let toml_string = toml::from_str::<ABCIConfigStr>(&file_contents).c(d!())?;
        let config = ABCIConfig::try_from(toml_string).c(d!())?;
        Ok(config)
    }
}

pub mod global_cfg {
    use crate::abci::CheckPointConfig;
    #[cfg(target_os = "linux")]
    use btm::BtmCfg;
    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    use btm::{SnapAlgo, SnapMode, STEP_CNT};
    #[cfg(not(test))]
    use clap::{crate_authors, App, Arg, ArgMatches};
    use lazy_static::lazy_static;
    use ruc::*;
    #[cfg(not(test))]
    use std::{env, process::exit};

    lazy_static! {
        /// Global abci config.
        pub static ref CFG: Config = pnk!(get_config());
    }

    #[derive(Default)]
    pub struct Config {
        pub abci_host: String,
        pub abci_port: u16,
        pub enable_enterprise_web3: bool,
        pub arc_history: (u16, Option<u16>),
        pub arc_fresh: bool,
        pub tendermint_host: String,
        pub tendermint_port: u16,
        pub submission_service_port: u16,
        pub ledger_service_port: u16,
        pub enable_query_service: bool,
        pub disable_eth_empty_blocks: bool,
        pub enable_eth_api_service: bool,
        pub evm_http_port: u16,
        pub evm_ws_port: u16,
        pub tendermint_node_self_addr: Option<String>,
        pub tendermint_node_key_config_path: Option<String>,
        pub ledger_dir: String,
        #[cfg(target_os = "linux")]
        pub btmcfg: BtmCfg,
        pub checkpoint: CheckPointConfig,
    }

    #[cfg(test)]
    fn get_config() -> Result<Config> {
        Ok(Config {
            ledger_dir: globutils::fresh_tmp_dir().to_string_lossy().into_owned(),
            ..Default::default()
        })
    }

    #[cfg(not(test))]
    fn get_config() -> Result<Config> {
        let m = App::new("abcid")
            .version(env!("VERGEN_SHA"))
            .author(crate_authors!())
            .about("An ABCI node implementation of FindoraNetwork.")
            .arg_from_usage("--abcid-host=[ABCId IP]")
            .arg_from_usage("--abcid-port=[ABCId Port]")
            .arg_from_usage("--enable-enterprise-web3 'enable enterprise-web3'")
            .arg_from_usage("--arc-history=[EVM archive node tracing history, format \"PERIOD,INTERVAL\" in days]")
            .arg_from_usage("--arc-fresh 'EVM archive node with fresh tracing history'")
            .arg_from_usage("--tendermint-host=[Tendermint IP]")
            .arg_from_usage("--tendermint-port=[Tendermint Port]")
            .arg_from_usage("--submission-service-port=[Submission Service Port]")
            .arg_from_usage("--ledger-service-port=[Ledger Service Port]")
            .arg_from_usage("-q, --enable-query-service")
            .arg_from_usage("--disable-eth-empty-blocks 'not generate empty ethereum blocks when no evm transaction'")
            .arg_from_usage("--enable-eth-api-service")
            .arg_from_usage("--evm-http-port=[EVM Web3 Http Port]")
            .arg_from_usage("--evm-ws-port=[EVM Web3 WS Port]")
            .arg_from_usage("--tendermint-node-self-addr=[Address] 'the address of your tendermint node, in upper-hex format'")
            .arg_from_usage("--tendermint-node-key-config-path=[Path] 'such as: ${HOME}/.tendermint/config/priv_validator_key.json'")
            .arg_from_usage("-d, --ledger-dir=[Path]")
            .arg_from_usage("--checkpoint-file=[Path]")
            .arg_from_usage("--enable-snapshot 'global switch for enabling snapshot functions'")
            .arg_from_usage("--snapshot-list 'list all available snapshots in the form of block height'")
            .arg_from_usage("--snapshot-target=[TargetPath] 'a data volume containing both ledger data and tendermint data'")
            .arg_from_usage("--snapshot-itv=[Iterval] 'interval between adjacent snapshots, default to 10 blocks'")
            .arg_from_usage("--snapshot-cap=[Capacity] 'the maximum number of snapshots that will be stored, default to 100'")
            .arg_from_usage("--snapshot-mode=[Mode] 'zfs/btrfs/external, will try a guess if missing'")
            .arg_from_usage("--snapshot-algo=[Algo] 'fair/fade, default to `fair`'")
            .arg_from_usage("--snapshot-rollback 'rollback to the last available snapshot'")
            .arg_from_usage("-r, --snapshot-rollback-to=[Height] 'rollback to a custom height, will try the closest smaller height if the target does not exist'")
            .arg_from_usage("-R, --snapshot-rollback-to-exact=[Height] 'rollback to a custom height exactly, an error will be reported if the target does not exist'")
            .arg(Arg::with_name("_a").long("ignored").hidden(true))
            .arg(Arg::with_name("_b").long("nocapture").hidden(true))
            .arg(Arg::with_name("_c").long("test-threads").hidden(true))
            .arg(Arg::with_name("INPUT").multiple(true).hidden(true))
            .get_matches();

        print_version(&m);

        let ah = m
            .value_of("abcid-host")
            .map(|v| v.to_owned())
            .or_else(|| env::var("ABCI_HOST").ok())
            .unwrap_or_else(|| "0.0.0.0".to_owned());
        let ap = m
            .value_of("abcid-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("ABCI_PORT").ok())
            .unwrap_or_else(|| "26658".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let enable_enterprise_web3 = m.is_present("enable-enterprise-web3");
        let arh = {
            let trace = m
                .value_of("arc-history")
                .map(|v| v.to_owned())
                .or_else(|| env::var("ARC_HISTORY").ok())
                .unwrap_or_else(|| "90,10".to_string())
                .trim()
                .to_owned();
            if trace.is_empty() {
                return Err(eg!("empty trace"));
            }
            if trace.contains(',') {
                let t = trace.split(',').collect::<Vec<_>>();
                let trace = t
                    .first()
                    .expect("missing trace period")
                    .parse::<u16>()
                    .c(d!("invalid trace period"))?;
                let interval = Some(
                    t.get(1)
                        .expect("missing trace interval")
                        .parse::<u16>()
                        .c(d!("invalid trace interval"))?,
                );
                (trace, interval)
            } else if !trace.is_empty() {
                let trace = trace.parse::<u16>().c(d!("invalid trace period"))?;
                (trace, None)
            } else {
                return Err(eg!("invalid trace"));
            }
        };
        let arf = m.is_present("arc-fresh");
        let th = m
            .value_of("tendermint-host")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_HOST").ok())
            .unwrap_or_else(|| "0.0.0.0".to_owned());
        let tp = m
            .value_of("tendermint-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_PORT").ok())
            .unwrap_or_else(|| "26657".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let ssp = m
            .value_of("submission-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("SUBMISSION_PORT").ok())
            .unwrap_or_else(|| "8669".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let lsp = m
            .value_of("ledger-service-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("LEDGER_PORT").ok())
            .unwrap_or_else(|| "8668".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let eqs = m.is_present("enable-query-service")
            || env::var("ENABLE_QUERY_SERVICE").is_ok();
        let tnsa = m
            .value_of("tendermint-node-self-addr")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TD_NODE_SELF_ADDR").ok());
        let tnkcp = m
            .value_of("tendermint-node-key-abci-path")
            .map(|v| v.to_owned())
            .or_else(|| env::var("TENDERMINT_NODE_KEY_CONFIG_PATH").ok());
        let ld = m
            .value_of("ledger-dir")
            .map(|v| v.to_owned())
            .unwrap_or_else(|| {
                env::var("LEDGER_DIR").unwrap_or_else(|_| {
                    format!("{}/.tendermint/__findora__", pnk!(env::var("HOME")))
                })
            });
        let eeb = m.is_present("disable-eth-empty-blocks")
            || env::var("DISABLE_ETH_EMPTY_BLOCKS").is_ok();
        let eas = m.is_present("enable-eth-api-service")
            || env::var("ENABLE_ETH_API_SERVICE").is_ok();
        let ehp = m
            .value_of("evm-http-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_HTTP_PORT").ok())
            .unwrap_or_else(|| "8545".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let ewp = m
            .value_of("evm-ws-port")
            .map(|v| v.to_owned())
            .or_else(|| env::var("EVM_WS_PORT").ok())
            .unwrap_or_else(|| "8546".to_owned())
            .parse::<u16>()
            .c(d!())?;
        let checkpoint_path = m
            .value_of("checkpoint-file")
            .map(|v| v.to_owned())
            .unwrap_or_else(|| String::from("./checkpoint.toml"));

        let res = Config {
            abci_host: ah,
            abci_port: ap,
            enable_enterprise_web3,
            arc_history: arh,
            arc_fresh: arf,
            tendermint_host: th,
            tendermint_port: tp,
            submission_service_port: ssp,
            ledger_service_port: lsp,
            enable_query_service: eqs,
            disable_eth_empty_blocks: eeb,
            enable_eth_api_service: eas,
            evm_http_port: ehp,
            evm_ws_port: ewp,
            tendermint_node_self_addr: tnsa,
            tendermint_node_key_config_path: tnkcp,
            ledger_dir: ld,
            #[cfg(target_os = "linux")]
            btmcfg: parse_btmcfg(&m).c(d!())?,
            checkpoint: CheckPointConfig::from_file(&checkpoint_path).unwrap(),
        };

        Ok(res)
    }

    #[cfg(not(test))]
    fn print_version(m: &ArgMatches) {
        if m.is_present("version") {
            println!("{}", env!("VERGEN_SHA"));
            exit(0);
        }
    }

    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    fn parse_btmcfg(m: &ArgMatches) -> Result<BtmCfg> {
        let mut res = BtmCfg::new();

        res.enable = m.is_present("enable-snapshot");

        if res.enable {
            res.itv = m
                .value_of("snapshot-itv")
                .unwrap_or("10")
                .parse::<u64>()
                .c(d!())?;
            res.cap = m
                .value_of("snapshot-cap")
                .unwrap_or("100")
                .parse::<u64>()
                .c(d!())?;

            if let Some(sm) = m.value_of("snapshot-mode") {
                res.mode = SnapMode::from_string(sm).c(d!())?;
                if !matches!(res.mode, SnapMode::External) {
                    res.target = m.value_of("snapshot-target").c(d!())?.to_owned();
                }
            } else {
                res.target = m.value_of("snapshot-target").c(d!())?.to_owned();
                res.mode = res.guess_mode().c(d!())?;
            }

            if let Some(sa) = m.value_of("snapshot-algo") {
                res.algo = SnapAlgo::from_string(sa).c(d!())?;
                res.itv.checked_pow(STEP_CNT as u32).c(d!())?;
            }
        }

        if m.is_present("snapshot-list")
            || m.is_present("snapshot-rollback")
            || m.is_present("snapshot-rollback-to")
            || m.is_present("snapshot-rollback-to-exact")
        {
            // this field should be parsed at the top
            res.target = m.value_of("snapshot-target").c(d!())?.to_owned();

            // the guess should always success in this scene
            res.mode = res.guess_mode().c(d!())?;

            if m.is_present("snapshot-list") {
                list_snapshots(&res).c(d!())?;
            }

            check_rollback(m, &res).c(d!())?;
        }

        Ok(res)
    }

    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    fn list_snapshots(cfg: &BtmCfg) -> Result<()> {
        println!("Available snapshots are listed below:");
        cfg.get_sorted_snapshots()
            .c(d!())?
            .into_iter()
            .rev()
            .for_each(|h| {
                println!("    {h}",);
            });
        exit(0);
    }

    #[cfg(not(test))]
    #[cfg(target_os = "linux")]
    fn check_rollback(m: &ArgMatches, cfg: &BtmCfg) -> Result<()> {
        const HINTS: &str = r#"    NOTE:
            before executing the rollback,
            all related processes must be exited,
            such as findorad, abcid, tendermint, etc.
        "#;

        if m.is_present("snapshot-rollback")
            || m.is_present("snapshot-rollback-to")
            || m.is_present("snapshot-rollback-to-exact")
        {
            println!("\x1b[31;01m\n{HINTS}\x1b[00m");

            let (h, strict) = m
                .value_of("snapshot-rollback-to-exact")
                .map(|h| (Some(h), true))
                .or_else(|| m.value_of("snapshot-rollback-to").map(|h| (Some(h), false)))
                .unwrap_or((None, false));
            let h = if let Some(h) = h {
                Some(h.parse::<u64>().c(d!())?)
            } else {
                None
            };
            cfg.rollback(h, strict).c(d!())?;

            exit(0);
        }
        Ok(())
    }
}
