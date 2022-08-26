pub trait Get2<T1, T2> {
    /// Return the current value.
    fn get(ctx: T2) -> T1;
}

// impl<T: Default> Get2<T> for () {
//     fn get(_ctx: Contex2) -> T {
//         T::default()
//     }
// }

/// A trait for querying a single value from a type.
///
/// It is not required that the value is constant.
pub trait Get<T> {
    /// Return the current value.
    fn get() -> T;
}

impl<T: Default> Get<T> for () {
    fn get() -> T {
        T::default()
    }
}

/// # Examples
///
/// ```
/// # use fp_core::macros::Get;
/// # use fp_core::parameter_types;
/// // This function cannot be used in a const context.
/// fn non_const_expression() -> u64 { 99 }
///
/// const FIXED_VALUE: u64 = 10;
/// parameter_types! {
///    pub const Argument: u64 = 42;
///    /// Visibility of the type is optional
///    OtherArgument: u64 = non_const_expression();
/// }
///
/// trait Config {
///    type Parameter: Get<u64>;
///    type OtherParameter: Get<u64>;
/// }
///
/// struct Module;
/// impl Config for Module {
///    type Parameter = Argument;
///    type OtherParameter = OtherArgument;
/// }
///
/// ```
#[macro_export]
macro_rules! parameter_types {
    (
        $( #[ $attr:meta ] )*
        $vis:vis const $name:ident: $type:ty = $value:expr;
        $( $rest:tt )*
    ) => (
        $( #[ $attr ] )*
        $vis struct $name;
        $crate::parameter_types!(IMPL_CONST $name , $type , $value);
        $crate::parameter_types!( $( $rest )* );
    );
    (
        $( #[ $attr:meta ] )*
        $vis:vis $name:ident: $type:ty = $value:expr;
        $( $rest:tt )*
    ) => (
        $( #[ $attr ] )*
        $vis struct $name;
        $crate::parameter_types!(IMPL $name, $type, $value);
        $crate::parameter_types!( $( $rest )* );
    );
    () => ();
    (IMPL_CONST $name:ident, $type:ty, $value:expr) => {
        impl $name {
            /// Returns the value of this parameter type.
            pub const fn get() -> $type {
                $value
            }
        }

        impl<I: From<$type>> $crate::macros::Get<I> for $name {
            fn get() -> I {
                I::from($value)
            }
        }
    };
    (IMPL $name:ident, $type:ty, $value:expr) => {
        impl $name {
            /// Returns the value of this parameter type.
            pub fn get() -> $type {
                $value
            }
        }

        impl<I: From<$type>> $crate::macros::Get<I> for $name {
            fn get() -> I {
                I::from($value)
            }
        }
    };
}

/// Return Err of the expression: `return Err($expression);`.
///
/// Used as `fail!(expression)`.
#[macro_export]
macro_rules! fail {
    ( $y:expr ) => {{
        return Err(eg!($y));
    }};
}

/// Evaluate `$x:expr` and if not true return `Err($y:expr)`.
///
/// Used as `ensure!(expression_to_ensure, expression_to_return_on_false)`.
#[macro_export]
macro_rules! ensure {
    ( $x:expr, $y:expr $(,)? ) => {{
        if !$x {
            $crate::fail!($y);
        }
    }};
}
