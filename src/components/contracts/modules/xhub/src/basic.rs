use super::{App, Config, MODULE_NAME};
use fp_core::module::AppModuleBasic;
use ruc::Result;

impl<C: Config> AppModuleBasic for App<C> {
    fn name() -> String {
        MODULE_NAME.into()
    }

    fn default_genesis(&self) -> Vec<u8> {
        todo!()
    }

    fn init_genesis(&self) {
        todo!()
    }

    fn validate_genesis(&self) -> Result<()> {
        todo!()
    }

    fn export_genesis(&self) {
        todo!()
    }
}
