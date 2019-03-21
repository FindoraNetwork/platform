extern crate serde;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate arrayref;

pub mod store;
pub mod data_model;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
