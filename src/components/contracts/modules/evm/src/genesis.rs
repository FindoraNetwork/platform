use super::{App, Config};
use fp_core::module::AppModuleGenesis;

impl<C: Config> AppModuleGenesis for App<C> {
    fn init_genesis(&self) {
        todo!()
    }

    fn export_genesis(&self) {
        todo!()
    }
}
