/// Prefix handling
pub const SPLIT_BGN: &str = "_";
pub const SPLIT_END: &str = "~";

#[derive(Clone)]
pub struct Prefix {
    base: Vec<u8>,
}

impl Prefix {
    /// create base prefix
    pub fn new(key: &[u8]) -> Self {
        Prefix { base: key.to_vec() }
    }

    /// adds a key or sub prefix to base
    ///
    /// "base" ==> "base_key"
    pub fn push(&self, key: &[u8]) -> Self {
        let mut pfx = self.clone();
        pfx.base.extend_from_slice(SPLIT_BGN.as_bytes());
        pfx.base.extend_from_slice(key);
        pfx
    }

    /// adds sub prefix and key to base
    ///
    /// "base" ==> "base_subprefix_key"
    ///
    /// useful for ranged keys
    pub fn push_sub(&self, sub_prefix: &[u8], sub_key: &[u8]) -> Self {
        let pfx = self.clone();
        pfx.push(sub_prefix).push(sub_key)
    }

    /// get lower bound prefix
    ///
    /// "base_subprefix" ==> "base_subprefix_"
    pub fn begin(&self) -> Vec<u8> {
        let mut key_begin = self.base.clone();
        key_begin.extend_from_slice(SPLIT_BGN.as_bytes());
        key_begin
    }

    /// get upper bound prefix
    ///
    /// "base_subprefix" ==> "base_subprefix~"
    pub fn end(&self) -> Vec<u8> {
        let mut key_end = self.base.clone();
        key_end.extend_from_slice(SPLIT_END.as_bytes());
        key_end
    }
}

impl AsRef<[u8]> for Prefix {
    fn as_ref(&self) -> &[u8] {
        self.base.as_ref()
    }
}

impl From<Vec<u8>> for Prefix {
    fn from(item: Vec<u8>) -> Self {
        Prefix::new(item.as_ref())
    }
}

/// convert to string
impl ToString for Prefix {
    fn to_string(&self) -> String {
        std::str::from_utf8(&self.base).map_or("".to_owned(), |text| text.to_owned())
    }
}
