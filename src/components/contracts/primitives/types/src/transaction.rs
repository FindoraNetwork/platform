use crate::{
    assemble::OptionalHash,
    crypto::{IdentifyAccount, Verify},
};
use core::fmt::Debug;
use ruc::*;
use serde::{Deserialize, Serialize};

/// This is unchecked and so can contain a signature.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct UncheckedTransaction<Address, Call, Signature, Extra> {
    /// The signature is to use the Address sign the function if this is a signed transaction.
    pub signature: Option<(Address, Signature, Extra)>,
    /// The function that should be called.
    pub function: Call,
    #[serde(skip_serializing_if = "OptionalHash::is_none")]
    #[serde(default = "OptionalHash::default")]
    pub hash: OptionalHash,
}

impl<Address, Call, Signature, Extra>
    UncheckedTransaction<Address, Call, Signature, Extra>
{
    pub fn new_signed(
        function: Call,
        signed: Address,
        signature: Signature,
        extra: Extra,
    ) -> Self {
        Self {
            signature: Some((signed, signature, extra)),
            function,
            hash: OptionalHash::None,
        }
    }

    pub fn new_unsigned(function: Call) -> Self {
        Self {
            signature: None,
            function,
            hash: OptionalHash::None,
        }
    }
}

impl<Address, Call, Signature, Extra>
    UncheckedTransaction<Address, Call, Signature, Extra>
where
    Call: Serialize + Clone,
    Signature: Verify,
    <Signature as Verify>::Signer: IdentifyAccount<AccountId = Address>,
    Extra: Serialize + Clone,
{
    pub fn check(self) -> Result<CheckedTransaction<Address, Call, Extra>> {
        Ok(match self.signature {
            Some((signed, signature, extra)) => {
                let msg =
                    serde_json::to_vec(&(self.function.clone(), extra.clone())).unwrap();

                if !signature.verify(msg.as_slice(), &signed) {
                    return Err(eg!("bad transaction signature"));
                }

                CheckedTransaction {
                    signed: Some((signed, extra)),
                    function: self.function,
                    hash: self.hash,
                }
            }
            None => CheckedTransaction {
                signed: None,
                function: self.function,
                hash: self.hash,
            },
        })
    }
}

/// It has been checked and is good, particularly with regards to the signature.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CheckedTransaction<Address, Call, Extra> {
    /// The function signer, if anyone
    pub signed: Option<(Address, Extra)>,

    /// The function that should be called.
    pub function: Call,

    ///hash of raw tx
    #[serde(skip_serializing_if = "OptionalHash::is_none")]
    pub hash: OptionalHash,
}
