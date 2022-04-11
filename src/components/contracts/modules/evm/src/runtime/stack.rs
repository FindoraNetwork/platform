use crate::{storage::*, AddressMapping, App, Config};
use config::abci::global_cfg::CFG;
use ethereum_types::{H160, H256, U256};
use evm::{
    backend::Backend,
    executor::{StackState, StackSubstateMetadata},
    ExitError, Transfer,
};
use fp_core::{context::Context, macros::Get};
use fp_evm::{Log, Vicinity};
use fp_storage::{BorrowMut, DerefMut};
use fp_traits::{account::AccountAsset, evm::BlockHashMapping};
use fp_utils::timestamp_converter;
use log::info;
use std::{collections::btree_set::BTreeSet, marker::PhantomData, mem};
use storage::{db::FinDB, state::State};

pub struct FindoraStackSubstate<'context, 'config> {
    pub ctx: &'context Context,
    pub metadata: StackSubstateMetadata<'config>,
    pub deletes: BTreeSet<H160>,
    pub logs: Vec<Log>,
    pub parent: Option<Box<FindoraStackSubstate<'context, 'config>>>,
    pub substate: State<FinDB>,
}

impl<'context, 'config> FindoraStackSubstate<'context, 'config> {
    pub fn metadata(&self) -> &StackSubstateMetadata<'config> {
        &self.metadata
    }

    pub fn metadata_mut(&mut self) -> &mut StackSubstateMetadata<'config> {
        &mut self.metadata
    }

    pub fn enter(&mut self, gas_limit: u64, is_static: bool) {
        let substate = (*self.ctx.state.read()).substate();

        let mut entering = Self {
            ctx: self.ctx,
            metadata: self.metadata.spit_child(gas_limit, is_static),
            parent: None,
            deletes: BTreeSet::new(),
            logs: Vec::new(),
            substate,
        };
        mem::swap(&mut entering, self);

        self.parent = Some(Box::new(entering));
    }

    pub fn exit_commit(&mut self) -> Result<(), ExitError> {
        let mut exited = *self.parent.take().expect("Cannot commit on root substate");
        mem::swap(&mut exited, self);

        self.metadata.swallow_commit(exited.metadata)?;
        self.logs.append(&mut exited.logs);
        self.deletes.append(&mut exited.deletes);

        Ok(())
    }

    pub fn exit_revert(&mut self) -> Result<(), ExitError> {
        let mut exited = *self.parent.take().expect("Cannot discard on root substate");
        mem::swap(&mut exited, self);
        self.metadata.swallow_revert(exited.metadata)?;

        if self.ctx.header.height >= CFG.checkpoint.evm_substate_height {
            let _ = mem::replace(self.ctx.state.write().deref_mut(), exited.substate);
        } else {
            info!(target: "evm", "EVM stack exit_revert(), height: {:?}", self.ctx.header.height);
        }

        Ok(())
    }

    pub fn exit_discard(&mut self) -> Result<(), ExitError> {
        let mut exited = *self.parent.take().expect("Cannot discard on root substate");
        mem::swap(&mut exited, self);
        self.metadata.swallow_discard(exited.metadata)?;

        if self.ctx.header.height >= CFG.checkpoint.evm_substate_height {
            let _ = mem::replace(self.ctx.state.write().deref_mut(), exited.substate);
        } else {
            info!(target: "evm", "EVM stack exit_discard(), height: {:?}", self.ctx.header.height);
        }

        Ok(())
    }

    pub fn deleted(&self, address: H160) -> bool {
        if self.deletes.contains(&address) {
            return true;
        }

        if let Some(parent) = self.parent.as_ref() {
            return parent.deleted(address);
        }

        false
    }

    pub fn set_deleted(&mut self, address: H160) {
        self.deletes.insert(address);
    }

    pub fn log(&mut self, address: H160, topics: Vec<H256>, data: Vec<u8>) {
        self.logs.push(Log {
            address,
            topics,
            data,
        });
    }
}

/// Findora backend for EVM.
pub struct FindoraStackState<'context, 'vicinity, 'config, T> {
    pub ctx: &'context Context,
    pub vicinity: &'vicinity Vicinity,
    pub substate: FindoraStackSubstate<'context, 'config>,
    _marker: PhantomData<T>,
}

impl<'context, 'vicinity, 'config, C: Config>
    FindoraStackState<'context, 'vicinity, 'config, C>
{
    /// Create a new backend with given vicinity.
    pub fn new(
        ctx: &'context Context,
        vicinity: &'vicinity Vicinity,
        metadata: StackSubstateMetadata<'config>,
    ) -> Self {
        let substate = (*ctx.state.read()).substate();
        Self {
            ctx,
            vicinity,
            substate: FindoraStackSubstate {
                ctx,
                metadata,
                deletes: BTreeSet::new(),
                logs: Vec::new(),
                parent: None,
                substate,
            },
            _marker: PhantomData,
        }
    }
}

impl<'context, 'vicinity, 'config, C: Config> Backend
    for FindoraStackState<'context, 'vicinity, 'config, C>
{
    fn gas_price(&self) -> U256 {
        self.vicinity.gas_price
    }

    fn origin(&self) -> H160 {
        self.vicinity.origin
    }

    fn block_hash(&self, number: U256) -> H256 {
        C::BlockHashMapping::block_hash(self.ctx, number).unwrap_or_default()
    }

    fn block_number(&self) -> U256 {
        U256::from(self.ctx.header.height)
    }

    fn block_coinbase(&self) -> H160 {
        App::<C>::find_proposer(self.ctx)
    }

    fn block_timestamp(&self) -> U256 {
        let block_timestamp = self.ctx.header.time.clone().unwrap_or_default();
        U256::from(timestamp_converter(block_timestamp))
    }

    fn block_difficulty(&self) -> U256 {
        U256::zero()
    }

    fn block_gas_limit(&self) -> U256 {
        C::BlockGasLimit::get()
    }

    fn chain_id(&self) -> U256 {
        U256::from(C::ChainId::get())
    }

    fn exists(&self, _address: H160) -> bool {
        true
    }

    fn basic(&self, address: H160) -> evm::backend::Basic {
        let account = App::<C>::account_basic(self.ctx, &address);

        evm::backend::Basic {
            balance: account.balance,
            nonce: account.nonce,
        }
    }

    fn code(&self, address: H160) -> Vec<u8> {
        App::<C>::account_codes(self.ctx, &address.into(), None).unwrap_or_default()
    }

    fn storage(&self, address: H160, index: H256) -> H256 {
        App::<C>::account_storages(self.ctx, &address.into(), &index.into(), None)
            .unwrap_or_default()
    }

    fn original_storage(&self, _address: H160, _index: H256) -> Option<H256> {
        None
    }
}

impl<'context, 'vicinity, 'config, C: Config> StackState<'config>
    for FindoraStackState<'context, 'vicinity, 'config, C>
{
    fn metadata(&self) -> &StackSubstateMetadata<'config> {
        self.substate.metadata()
    }

    fn metadata_mut(&mut self) -> &mut StackSubstateMetadata<'config> {
        self.substate.metadata_mut()
    }

    fn enter(&mut self, gas_limit: u64, is_static: bool) {
        self.substate.enter(gas_limit, is_static)
    }

    fn exit_commit(&mut self) -> Result<(), ExitError> {
        self.substate.exit_commit()
    }

    fn exit_revert(&mut self) -> Result<(), ExitError> {
        self.substate.exit_revert()
    }

    fn exit_discard(&mut self) -> Result<(), ExitError> {
        self.substate.exit_discard()
    }

    fn is_empty(&self, address: H160) -> bool {
        App::<C>::is_account_empty(self.ctx, &address.into())
    }

    fn deleted(&self, address: H160) -> bool {
        self.substate.deleted(address)
    }

    fn inc_nonce(&mut self, address: H160) {
        let account_id = C::AddressMapping::convert_to_account_id(address);
        let _ = C::AccountAsset::inc_nonce(self.ctx, &account_id);
    }

    fn set_storage(&mut self, address: H160, index: H256, value: H256) {
        if value == H256::default() {
            log::debug!(
                target: "evm",
                "Removing storage for {:?} [index: {:?}]",
                address,
                index,
            );
            AccountStorages::remove(
                self.ctx.state.write().borrow_mut(),
                &address.into(),
                &index.into(),
            );
        } else {
            log::debug!(
                target: "evm",
                "Updating storage for {:?} [index: {:?}, value: {:?}]",
                address,
                index,
                value,
            );
            if let Err(e) = AccountStorages::insert(
                self.ctx.state.write().borrow_mut(),
                &address.into(),
                &index.into(),
                &value,
            ) {
                log::error!(
                    target: "evm",
                    "Failed updating storage for {:?} [index: {:?}, value: {:?}], error: {:?}",
                    address,
                    index,
                    value,
                        e
                );
            }
        }
    }

    fn reset_storage(&mut self, address: H160) {
        AccountStorages::remove_prefix(
            self.ctx.state.write().borrow_mut(),
            &address.into(),
        );
    }

    fn log(&mut self, address: H160, topics: Vec<H256>, data: Vec<u8>) {
        self.substate.log(address, topics, data)
    }

    fn set_deleted(&mut self, address: H160) {
        self.substate.set_deleted(address)
    }

    fn set_code(&mut self, address: H160, code: Vec<u8>) {
        let code_len = code.len();
        log::debug!(
            target: "evm",
            "Inserting code ({} bytes) at {:?}",
           code_len,
            address
        );
        if let Err(e) = App::<C>::create_account(self.ctx, address.into(), code) {
            log::error!(
                target: "evm",
                "Failed inserting code ({} bytes) at {:?}, error: {:?}",
                code_len,
                address,
                    e
            );
        }
    }

    fn transfer(&mut self, transfer: Transfer) -> Result<(), ExitError> {
        let source = C::AddressMapping::convert_to_account_id(transfer.source);
        let target = C::AddressMapping::convert_to_account_id(transfer.target);

        let result =
            C::AccountAsset::transfer(self.ctx, &source, &target, transfer.value)
                .map_err(|_| ExitError::OutOfFund);

        result
    }

    fn reset_balance(&mut self, _address: H160) {
        // Do nothing on reset balance in Findora.
        //
        // This function exists in EVM because a design issue
        // (arguably a bug) in SELFDESTRUCT that can cause total
        // issurance to be reduced. We do not need to replicate this.
    }

    fn touch(&mut self, _address: H160) {
        // Do nothing on touch in Findora.
        //
        // EVM module considers all accounts to exist, and distinguish
        // only empty and non-empty accounts. This avoids many of the
        // subtle issues in EIP-161.
    }
}
