//! Implementation of  fraction represent for calculate.

use core::cmp::Ordering;
use core::fmt;
use core::ops::{Add, Mul, Sub};

use ruc::*;

use core::convert::TryFrom;

type Repr = u128;

///The unsigned fraction, represent as Tuple(numerator, denominator).
#[derive(Debug, Clone, Copy)]
pub struct U128Fraction(Repr, Repr);

impl fmt::Display for U128Fraction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 != 0 {
            write!(f, "{}/{}", self.0, self.1)
        } else {
            write!(f, "0")
        }
    }
}

///Greatest common divisor, use "Euclidean Algorithm".
fn gcd(mut x: Repr, mut y: Repr) -> Repr {
    while y != 0 {
        let t = y;
        y = x % y;
        x = t;
    }
    x
}

impl U128Fraction {
    ///Give out a zero.
    pub fn zero() -> Self {
        U128Fraction(0, 1)
    }

    ///Construct a fraction by a numerator and denominator, **Error** would be returned if denominator is zero.
    pub fn new(n: Repr, d: Repr) -> Result<Self> {
        if d == 0 {
            return Err(eg!("Denominator is zero!"));
        }

        Ok(U128Fraction::new_unchecked(n, d))
    }

    fn new_unchecked(n: Repr, d: Repr) -> Self {
        if n == 0 {
            return U128Fraction::zero();
        }
        let g = gcd(n, d);
        U128Fraction(n / g, d / g)
    }

    ///Estimate the value of the fracton.
    pub fn estimate_f64(&self) -> f64 {
        self.0 as f64 / self.1 as f64
    }

    ///Return the quotient and remainder of this fraction.
    pub fn quotient(&self) -> (Repr, Repr) {
        (self.0 / self.1, self.0 % self.1)
    }

    ///Consume itself and return a `[Repr;2]`.
    pub fn into_raw(self) -> [Repr; 2] {
        [self.0, self.1]
    }

    ///Calculate: a/b <op> c/d = (ad <op> bc)/(bd)
    fn saturating_op(self, rhs: Self, op: impl Fn(Repr, Repr) -> Repr) -> Self {
        if self.1 == rhs.1 {
            return U128Fraction::new_unchecked(op(self.0, rhs.0), self.1);
        }

        let num = op(self.0.saturating_mul(rhs.1), self.1.saturating_mul(rhs.0));
        let div = Repr::saturating_mul(self.1, rhs.1);
        U128Fraction::new_unchecked(num, div)
    }

    ///Saturating addition with an U64Frac. Computes `self + rhs`.
    pub fn saturating_add(self, rhs: Self) -> Self {
        self.saturating_op(rhs, Repr::saturating_add)
    }

    ///Saturating substraction with an U64Frac. Computes `self - rhs`.
    pub fn saturating_sub(self, rhs: Self) -> Self {
        self.saturating_op(rhs, Repr::saturating_sub)
    }

    ///Saturating multiplication with an U64Frac. Computes `self * rhs`.
    pub fn saturating_mul(self, rhs: Self) -> Self {
        if self.0 == 0 || rhs.0 == 0 {
            return U128Fraction::zero();
        }
        let num = self.0.saturating_mul(rhs.0);
        let div = self.1.saturating_mul(rhs.1);
        U128Fraction::new_unchecked(num, div)
    }
}

impl PartialEq for U128Fraction {
    fn eq(&self, other: &Self) -> bool {
        if self.0 == 0 {
            return other.0 == 0;
        }
        self.0 == other.0 && self.1 == other.1
    }
}

impl Eq for U128Fraction {}

impl PartialOrd for U128Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(
            Repr::saturating_mul(self.0, other.1)
                .cmp(&Repr::saturating_mul(self.1, other.0)),
        )
    }
}

impl Ord for U128Fraction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Add<Repr> for U128Fraction {
    type Output = Self;

    fn add(self, other: Repr) -> Self {
        U128Fraction::new_unchecked(
            Repr::saturating_add(self.0, other.saturating_mul(self.1)),
            self.1,
        )
    }
}

impl Sub<Repr> for U128Fraction {
    type Output = Self;

    fn sub(self, other: Repr) -> Self {
        U128Fraction::new_unchecked(
            Repr::saturating_sub(self.0, other.saturating_mul(self.1)),
            self.1,
        )
    }
}

impl Mul<Repr> for U128Fraction {
    type Output = Self;

    fn mul(self, other: Repr) -> Self {
        U128Fraction::new_unchecked(self.0.saturating_mul(other), self.1)
    }
}

impl Mul<U128Fraction> for u128 {
    type Output = U128Fraction;
    fn mul(self, other: U128Fraction) -> Self::Output {
        other * self
    }
}

impl TryFrom<[Repr; 2]> for U128Fraction {
    type Error = Box<dyn RucError>;

    fn try_from(other: [Repr; 2]) -> Result<Self> {
        U128Fraction::new(other[0], other[1])
    }
}

impl TryFrom<(Repr, Repr)> for U128Fraction {
    type Error = Box<dyn RucError>;

    fn try_from(other: (Repr, Repr)) -> Result<Self> {
        U128Fraction::new(other.0, other.1)
    }
}

impl From<Repr> for U128Fraction {
    fn from(rhs: Repr) -> Self {
        U128Fraction::new_unchecked(rhs, 1)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_calculation() {
        //get 1/2
        let half = U128Fraction::new(125, 250).unwrap();
        assert_eq!(half.saturating_sub(half), U128Fraction::zero());
        // 1/4 = 1/2 * 1/2
        let quarter = half.saturating_mul(half);
        println!("{}", quarter);
        // 1/4 = 1/2 - 1/4
        assert_eq!(quarter, half.saturating_sub(quarter));
        assert!((quarter.estimate_f64() - 0.25_f64).abs() < f64::EPSILON);
        // 1/4 < 1/2
        assert!(quarter < half);

        let x = quarter * 7_u128;
        println!("{}", x);
        assert!(x > half);

        // 7/4 = 1/4 + 3/2
        assert_eq!(x, quarter.saturating_add(U128Fraction::new(3, 2).unwrap()));
        // 7/4 + 1 = 11/4
        assert_eq!(x + 1, U128Fraction::new(11, 4).unwrap());
        // 7/4 = 1, remainder = 3
        let (q, r) = x.quotient();
        assert_eq!(q, 1);
        assert_eq!(r, 3);
    }
}
