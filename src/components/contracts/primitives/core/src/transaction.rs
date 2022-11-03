use crate::context::{Context, RunTxMode};
use abci::Event;
use config::abci::global_cfg::CFG;
use fp_types::{crypto::Address, transaction::CheckedTransaction};
use impl_trait_for_tuples::impl_for_tuples;
use ruc::*;
use std::fmt::Debug;

/// A action (module function and argument values) that can be executed.
pub trait Executable {
    /// The caller
    type Origin;
    /// The call to execute
    type Call;

    /// Actually execute this action and return the result of it.
    fn execute(
        origin: Option<Self::Origin>,
        call: Self::Call,
        ctx: &Context,
    ) -> Result<ActionResult>;
}

/// Means by which a transaction may be extended.
pub trait SignedExtension: Sized {
    /// The type which encodes the sender identity.
    type AccountId;
    /// The type that encodes information that can be passed from pre_execute to post_execute.
    type Pre: Default;

    /// Validate a signed transaction for the transaction queue.
    fn validate(&self, _ctx: &Context, _who: &Self::AccountId) -> Result<()> {
        Ok(())
    }

    /// Do any pre-flight stuff for a signed transaction.
    ///
    /// For example: pre-pay tx fee, increase nonce...
    fn pre_execute(self, ctx: &Context, who: &Self::AccountId) -> Result<Self::Pre> {
        self.validate(ctx, who)?;
        Ok(Self::Pre::default())
    }

    /// Do any post-flight stuff for an transaction.
    ///
    /// For example: if the tx fee is left, the excess fee will be refunded.
    fn post_execute(
        _ctx: &Context,
        _pre: Self::Pre,
        _result: &ActionResult,
    ) -> Result<()> {
        Ok(())
    }
}

#[allow(clippy::type_complexity)]
#[impl_for_tuples(1, 12)]
impl<Address> SignedExtension for Tuple {
    for_tuples!( where #( Tuple: SignedExtension<AccountId=Address> )* );
    type AccountId = Address;
    for_tuples!( type Pre = ( #( Tuple::Pre ),* ); );

    fn validate(&self, ctx: &Context, who: &Self::AccountId) -> Result<()> {
        for_tuples!( #( Tuple.validate(ctx, who)?; )* );
        Ok(())
    }

    fn pre_execute(self, ctx: &Context, who: &Self::AccountId) -> Result<Self::Pre> {
        Ok(for_tuples!( ( #( Tuple.pre_execute(ctx, who)? ),* ) ))
    }

    fn post_execute(ctx: &Context, pre: Self::Pre, result: &ActionResult) -> Result<()> {
        for_tuples!( #( Tuple::post_execute(ctx, pre.Tuple, result)?; )* );
        Ok(())
    }
}

/// An "executable" action used by the transaction.
pub trait Applyable {
    type Origin;
    /// Type by which we can execute. Restricts the `UnsignedValidator` type.
    type Call;

    /// Checks to see if this is a valid *transaction*.
    fn validate<V: ValidateUnsigned<Call = Self::Call>>(
        &self,
        ctx: &Context,
    ) -> Result<()>;

    /// Executes all necessary logic needed prior to execute and deconstructs into function call,
    /// index and sender.
    fn apply<
        V: ValidateUnsigned<Call = Self::Call>
            + Executable<Origin = Self::Origin, Call = Self::Call>,
    >(
        self,
        ctx: &Context,
    ) -> Result<ActionResult>;
}

/// Something that can validate unsigned transactions for the transaction pool.
///
/// Note that any checks done here are only used for determining the validity of
/// the transaction for the transaction pool.
/// During block execution phase one need to perform the same checks anyway,
/// since this function is not being called.
pub trait ValidateUnsigned {
    /// The call to validate
    type Call;

    /// Validate the call right before execute.
    ///
    /// Changes made to storage WILL be persisted if the call returns `Ok`.
    fn pre_execute(ctx: &Context, call: &Self::Call) -> Result<()>;

    /// Return the validity of the call
    ///
    /// Changes made to storage should be discarded by caller.
    fn validate_unsigned(ctx: &Context, call: &Self::Call) -> Result<()>;
}

impl<Address, Call, Extra> Applyable for CheckedTransaction<Address, Call, Extra>
where
    Address: Clone,
    Extra: SignedExtension<AccountId = Address> + Clone,
{
    type Origin = Address;
    type Call = Call;

    fn validate<U: ValidateUnsigned<Call = Self::Call>>(
        &self,
        ctx: &Context,
    ) -> Result<()> {
        if let Some((ref sender, ref extra)) = self.signed.clone() {
            extra.validate(ctx, sender)
        } else {
            U::validate_unsigned(ctx, &self.function)
        }
    }

    fn apply<U>(self, ctx: &Context) -> Result<ActionResult>
    where
        U: ValidateUnsigned<Call = Self::Call>,
        U: Executable<Origin = Self::Origin, Call = Self::Call>,
    {
        let (maybe_who, pre) = if let Some((sender, extra)) = self.signed {
            let pre = Extra::pre_execute(extra, ctx, &sender)?;
            (Some(sender), pre)
        } else {
            U::pre_execute(ctx, &self.function)?;
            (None, Default::default())
        };

        ctx.state.write().commit_session();

        if ctx.header.height >= CFG.checkpoint.evm_checktx_nonce {
            // The transaction is only executed if the run mode is Deliver
            if ctx.run_mode == RunTxMode::Check {
                return Ok(ActionResult::default());
            }
        }

        match U::execute(maybe_who, self.function, ctx) {
            Ok(mut res) => {
                if !ctx.state.write().cache_mut().good2_commit() {
                    res.code = 0xff;
                    res.log = String::from("ctx state is not good to commit");

                    ctx.state.write().discard_session();
                } else {
                    Extra::post_execute(ctx, pre, &res)?;
                    ctx.state.write().commit_session();
                }

                ctx.db.write().commit_session();

                Ok(res)
            }
            Err(e) => {
                ctx.state.write().discard_session();
                ctx.db.write().discard_session();
                Err(e)
            }
        }
    }
}

/// Action execution result in the transaction.
#[derive(PartialEq, Debug, Clone, Default)]
pub struct ActionResult {
    /// code: 0 is succeed, others is failed
    /// 1 - Failed to execute EVM function
    /// 2 - EVM ExitReason::Error
    /// 3 - EVM ExitReason::Revert
    /// 4 - EVM ExitReason::Fatal
    /// 0xff - context state maybe messed up
    pub code: u32,
    /// Record the source address
    pub source: Option<Address>,
    /// Data is any data returned from message or handler execution.
    pub data: Vec<u8>,
    /// Log contains the log information from message or handler execution.
    pub log: String,
    /// gas_wanted is the maximum units of work we allow this tx to perform.
    pub gas_wanted: u64,
    /// gas_used is the amount of gas actually consumed.
    pub gas_used: u64,
    /// Events contains a slice of Event objects that were emitted during message
    /// or handler execution.
    pub events: Vec<Event>,
}
