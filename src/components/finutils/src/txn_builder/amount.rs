use std::convert::TryFrom;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

//use orchard::value as orchard;

pub const COIN: i64 = 1_0000_0000;
pub const MAX_MONEY: i64 = 21_000_000 * COIN;

//pub const DEFAULT_FEE: Amount = Amount(1000);

/// A type-safe representation of some quantity of Zcash.
///
/// An Amount can only be constructed from an integer that is within the valid monetary
/// range of `{-MAX_MONEY..MAX_MONEY}` (where `MAX_MONEY` = 21,000,000 × 10⁸ zatoshis).
/// However, this range is not preserved as an invariant internally; it is possible to
/// add two valid Amounts together to obtain an invalid Amount. It is the user's
/// responsibility to handle the result of serializing potentially-invalid Amounts. In
/// particular, a [`Transaction`] containing serialized invalid Amounts will be rejected
/// by the network consensus rules.
///
/// [`Transaction`]: crate::transaction::Transaction
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct Amount(i64);

/*
impl memuse::DynamicUsage for Amount {
    #[inline(always)]
    fn dynamic_usage(&self) -> usize {
        0
    }

    #[inline(always)]
    fn dynamic_usage_bounds(&self) -> (usize, Option<usize>) {
        (0, Some(0))
    }
}
*/
impl Amount {
    /// Returns a zero-valued Amount.
    pub const fn zero() -> Self {
        Amount(0)
    }

    /// Creates an Amount from an i64.
    ///
    /// Returns an error if the amount is outside the range `{-MAX_MONEY..MAX_MONEY}`.
    pub fn from_i64(amount: i64) -> Result<Self, ()> {
        if -MAX_MONEY <= amount && amount <= MAX_MONEY {
            Ok(Amount(amount))
        } else {
            Err(())
        }
    }

    /// Creates a non-negative Amount from an i64.
    ///
    /// Returns an error if the amount is outside the range `{0..MAX_MONEY}`.
    pub fn from_nonnegative_i64(amount: i64) -> Result<Self, ()> {
        if (0..=MAX_MONEY).contains(&amount) {
            Ok(Amount(amount))
        } else {
            Err(())
        }
    }

    /// Creates an Amount from a u64.
    ///
    /// Returns an error if the amount is outside the range `{0..MAX_MONEY}`.
    pub fn from_u64(amount: u64) -> Result<Self, ()> {
        if amount <= MAX_MONEY as u64 {
            Ok(Amount(amount as i64))
        } else {
            Err(())
        }
    }

    /// Reads an Amount from a signed 64-bit little-endian integer.
    ///
    /// Returns an error if the amount is outside the range `{-MAX_MONEY..MAX_MONEY}`.
    pub fn from_i64_le_bytes(bytes: [u8; 8]) -> Result<Self, ()> {
        let amount = i64::from_le_bytes(bytes);
        Amount::from_i64(amount)
    }

    /// Reads a non-negative Amount from a signed 64-bit little-endian integer.
    ///
    /// Returns an error if the amount is outside the range `{0..MAX_MONEY}`.
    pub fn from_nonnegative_i64_le_bytes(bytes: [u8; 8]) -> Result<Self, ()> {
        let amount = i64::from_le_bytes(bytes);
        Amount::from_nonnegative_i64(amount)
    }

    /// Reads an Amount from an unsigned 64-bit little-endian integer.
    ///
    /// Returns an error if the amount is outside the range `{0..MAX_MONEY}`.
    pub fn from_u64_le_bytes(bytes: [u8; 8]) -> Result<Self, ()> {
        let amount = u64::from_le_bytes(bytes);
        Amount::from_u64(amount)
    }

    /// Returns the Amount encoded as a signed 64-bit little-endian integer.
    pub fn to_i64_le_bytes(self) -> [u8; 8] {
        self.0.to_le_bytes()
    }

    /// Returns `true` if `self` is positive and `false` if the Amount is zero or
    /// negative.
    pub const fn is_positive(self) -> bool {
        self.0.is_positive()
    }

    /// Returns `true` if `self` is negative and `false` if the Amount is zero or
    /// positive.
    pub const fn is_negative(self) -> bool {
        self.0.is_negative()
    }
}

impl TryFrom<i64> for Amount {
    type Error = ();

    fn try_from(value: i64) -> Result<Self, ()> {
        Amount::from_i64(value)
    }
}

impl From<Amount> for i64 {
    fn from(amount: Amount) -> i64 {
        amount.0
    }
}

impl From<&Amount> for i64 {
    fn from(amount: &Amount) -> i64 {
        amount.0
    }
}

impl From<Amount> for u64 {
    fn from(amount: Amount) -> u64 {
        amount.0 as u64
    }
}

impl Add<Amount> for Amount {
    type Output = Option<Amount>;

    fn add(self, rhs: Amount) -> Option<Amount> {
        Amount::from_i64(self.0 + rhs.0).ok()
    }
}

impl Add<Amount> for Option<Amount> {
    type Output = Self;

    fn add(self, rhs: Amount) -> Option<Amount> {
        self.and_then(|lhs| lhs + rhs)
    }
}

impl AddAssign<Amount> for Amount {
    fn add_assign(&mut self, rhs: Amount) {
        *self = (*self + rhs).expect("Addition must produce a valid amount value.")
    }
}

impl Sub<Amount> for Amount {
    type Output = Option<Amount>;

    fn sub(self, rhs: Amount) -> Option<Amount> {
        Amount::from_i64(self.0 - rhs.0).ok()
    }
}

impl Sub<Amount> for Option<Amount> {
    type Output = Self;

    fn sub(self, rhs: Amount) -> Option<Amount> {
        self.and_then(|lhs| lhs - rhs)
    }
}

impl SubAssign<Amount> for Amount {
    fn sub_assign(&mut self, rhs: Amount) {
        *self = (*self - rhs).expect("Subtraction must produce a valid amount value.")
    }
}

impl Sum<Amount> for Option<Amount> {
    fn sum<I: Iterator<Item = Amount>>(iter: I) -> Self {
        iter.fold(Some(Amount::zero()), |acc, a| acc? + a)
    }
}

impl Neg for Amount {
    type Output = Self;

    fn neg(self) -> Self {
        Amount(-self.0)
    }
}