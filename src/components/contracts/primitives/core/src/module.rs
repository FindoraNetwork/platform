use crate::context::Context;
use abci::*;
use fp_types::U256;
use ruc::Result;

/// AppModuleBasic is the standard form for basic non-dependant elements of an application module.
pub trait AppModuleBasic {
    /// Returns the module's name.
    fn name() -> String;

    /// Returns default genesis state as raw bytes for the module.
    fn default_genesis(&self) -> Vec<u8>;

    /// Performs genesis initialization for the module. It returns no validator updates.
    fn init_genesis(&self);

    /// Performs genesis state validation for the module.
    fn validate_genesis(&self) -> Result<()>;

    /// Returns the exported genesis state as raw bytes for the module.
    fn export_genesis(&self);
}

/// AppModule is the standard form for an application module
pub trait AppModule: AppModuleBasic {
    /// query_route returns the application module's query response.
    fn query_route(
        &self,
        _ctx: Context,
        _path: Vec<&str>,
        _req: &RequestQuery,
    ) -> ResponseQuery {
        Default::default()
    }

    /// Tendermint consensus connection: called at the start of processing a block of transactions.
    fn begin_block(&mut self, _ctx: &mut Context, _req: &RequestBeginBlock) {}

    /// Tendermint consensus connection: called at the end of the block.
    fn end_block(
        &mut self,
        _ctx: &mut Context,
        _req: &RequestEndBlock,
        _ff_addr_balance: u64,
    ) -> ResponseEndBlock {
        Default::default()
    }

    fn commit(
        &mut self,
        _ctx: &mut Context,
        _height: U256,
        _root_hash: &[u8],
    ) -> Result<()> {
        Ok(())
    }
}
