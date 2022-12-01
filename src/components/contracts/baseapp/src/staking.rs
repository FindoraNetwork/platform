use ethereum_types::{H160, U256};
use fp_traits::account::AccountAsset;
use fp_traits::evm::{DecimalsMapping, EthereumDecimalsMapping};
use fp_types::crypto::Address;
use ledger::staking::evm::EVMStaking;

use crate::BaseApp;
use noah::xfr::sig::XfrPublicKey;
use ruc::{d, Result, RucResult};

impl EVMStaking for BaseApp {
    fn stake(
        &self,
        staker: &XfrPublicKey,
        amount: u64,
        td_addr: &[u8],
        td_pubkey: Vec<u8>,
        memo: String,
        rate: [u64; 2],
    ) -> Result<()> {
        let staker = mapping_address(staker);
        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        let from = H160::zero();

        module_account::App::<BaseApp>::mint(
            &self.deliver_state,
            &Address::from(from),
            amount,
        )?;

        if let Err(e) = self.modules.evm_module.stake(
            &self.deliver_state,
            from,
            amount,
            H160::from_slice(td_addr),
            td_pubkey,
            staker,
            memo,
            mapping_rate(rate),
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();
        Ok(())
    }

    fn delegate(
        &self,
        delegator: &XfrPublicKey,
        amount: u64,
        td_addr: &[u8],
    ) -> Result<()> {
        let delegator = mapping_address(delegator);
        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        let from = H160::zero();

        module_account::App::<BaseApp>::mint(
            &self.deliver_state,
            &Address::from(from),
            amount,
        )?;

        if let Err(e) = self.modules.evm_module.delegate(
            &self.deliver_state,
            from,
            H160::from_slice(td_addr),
            delegator,
            amount,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();

        Ok(())
    }

    fn undelegate(
        &self,
        delegator: &XfrPublicKey,
        td_addr: &[u8],
        amount: u64,
    ) -> Result<()> {
        let delegator = mapping_address(delegator);

        let from = H160::zero();

        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        if let Err(e) = self.modules.evm_module.undelegate(
            &self.deliver_state,
            from,
            H160::from_slice(td_addr),
            delegator,
            amount,
        ) {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        };

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();

        Ok(())
    }

    fn claim(&self, from: &XfrPublicKey, amount: u64) -> Result<()> {
        let from = mapping_address(from);
        let amount =
            EthereumDecimalsMapping::from_native_token(U256::from(amount)).c(d!())?;

        if let Err(e) = self
            .modules
            .evm_module
            .claim(&self.deliver_state, from, amount)
        {
            self.deliver_state.state.write().discard_session();
            self.deliver_state.db.write().discard_session();
            return Err(e);
        }

        self.deliver_state.state.write().commit_session();
        self.deliver_state.db.write().commit_session();
        Ok(())
    }
}

fn mapping_rate(rate: [u64; 2]) -> U256 {
    if rate[0] == 0 {
        return U256::zero();
    }

    let deciamls = 1_000_000_u64;
    U256::from(rate[0].saturating_mul(deciamls) / rate[1])
}

fn mapping_address(pk: &XfrPublicKey) -> H160 {
    use sha3::{Digest, Keccak256};
    let result = Keccak256::digest(&pk.to_bytes());
    H160::from_slice(&result.as_slice()[..20])
}
