use {anyhow::anyhow, ruc::err::Result, thiserror::Error};

#[derive(Error, Debug)]
pub enum WasmError {
    #[error("Invalid parameter {0}")]
    Param(String),
    #[error("{0} function handling error ")]
    Process(String),
    #[error("internal error {0}")]
    Inner(String),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type WasmResult<T> = std::result::Result<T, WasmError>;

pub trait WasmResulTrait<T> {
    fn to_wasm_result(self, prefix: &str) -> WasmResult<T>;
}

impl<T> WasmResulTrait<T> for Result<T> {
    #[inline(always)]
    fn to_wasm_result(self, prefix: &str) -> WasmResult<T> {
        self.map_err(|e| {
            WasmError::Other(anyhow!(
                "wasm process error:{}, {}",
                prefix,
                e.get_lowest_msg()
            ))
        })
    }
}
