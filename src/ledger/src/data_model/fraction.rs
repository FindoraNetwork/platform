//! Implementation of  fraction represent for calculate.

use core::cmp::Ordering;
use core::fmt;
use core::ops::{Add, Mul};

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

fn overflowing_add(a: Repr, b: Repr) -> Result<Repr> {
    let (r, o) = Repr::overflowing_add(a, b);
    if o {
        return Err(eg!("Addition overflowing!."));
    }
    Ok(r)
}

fn overflowing_sub(a: Repr, b: Repr) -> Result<Repr> {
    let (r, o) = Repr::overflowing_sub(a, b);
    if o {
        return Err(eg!("Substration overflowing!."));
    }
    Ok(r)
}

fn overflowing_mul(a: Repr, b: Repr) -> Result<Repr> {
    let (r, o) = Repr::overflowing_mul(a, b);
    if o {
        return Err(eg!("Multiplication overflowing!."));
    }
    Ok(r)
}

//overflowing never occur.
fn checked_div(a: Repr, b: Repr) -> Result<(Repr, Repr)> {
    if b == 0 {
        return Err(eg!("Dividing by zero!"));
    }
    Ok((a / b, a % b))
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
        if n == 0 {
            return Ok(U128Fraction::zero());
        }
        let g = gcd(n, d);
        Ok(U128Fraction(n / g, d / g))
    }

    ///Estimate the value of the fracton.
    pub fn estimate_f64(&self) -> f64 {
        self.0 as f64 / self.1 as f64
    }

    ///Return the quotient and remainder of this fraction, Error will be returned if denominator is zero.
    pub fn quotient(&self) -> Result<(Repr, Repr)> {
        checked_div(self.0, self.1)
    }

    ///Consume itself and return a `[Repr;2]`.
    pub fn into_raw(self) -> [Repr; 2] {
        [self.0, self.1]
    }

    ///Calculate: a/b <op> c/d = (ad <op> bc)/(bd)
    fn overflowing_op(
        self,
        rhs: Self,
        op: impl Fn(Repr, Repr) -> Result<Repr>,
    ) -> Result<Self> {
        if self.1 == rhs.1 {
            return U128Fraction::new(op(self.0, rhs.0)?, self.1);
        }

        let num = op(
            overflowing_mul(self.0, rhs.1)?,
            overflowing_mul(self.1, rhs.0)?,
        )?;
        let div = overflowing_mul(self.1, rhs.1)?;
        U128Fraction::new(num, div)
    }

    ///Overflowing addition with an U64Frac. Computes `self + rhs`, Error will be returned if overflowing.
    pub fn overflowing_add(self, rhs: Self) -> Result<Self> {
        self.overflowing_op(rhs, overflowing_add)
    }

    ///Overflowing substraction with an U64Frac. Computes `self - rhs`, Error will be returned if overflowing.
    pub fn overflowing_sub(self, rhs: Self) -> Result<Self> {
        self.overflowing_op(rhs, overflowing_sub)
    }

    ///Overflowing multiplication with an U64Frac. Computes `self * rhs`, Error will be returned if overflowing.
    pub fn overflowing_mul(self, rhs: Self) -> Result<Self> {
        if self.0 == 0 || rhs.0 == 0 {
            return Ok(U128Fraction::zero());
        }
        let num = overflowing_mul(self.0, rhs.0)?;
        let div = overflowing_mul(self.1, rhs.1)?;
        U128Fraction::new(num, div)
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
        let (r1, o1) = Repr::overflowing_mul(self.0, other.1);
        let (r2, o2) = Repr::overflowing_mul(self.1, other.0);
        if o1 || o2 {
            return None;
        }
        Some(r1.cmp(&r2))
    }
}

impl Add<Repr> for U128Fraction {
    type Output = Result<Self>;

    fn add(self, other: Repr) -> Self::Output {
        let n = overflowing_mul(self.1, other)?;
        U128Fraction::new(overflowing_add(self.0, n)?, self.1)
    }
}

impl Mul<Repr> for U128Fraction {
    type Output = Result<Self>;

    fn mul(self, other: Repr) -> Self::Output {
        U128Fraction::new(overflowing_mul(self.0, other)?, self.1)
    }
}

impl Mul<U128Fraction> for u128 {
    type Output = Result<U128Fraction>;
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
        U128Fraction::new(rhs, 1).unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_calculation() {
        //get 1/2
        let half = U128Fraction::new(125, 250).unwrap();
        assert_eq!(half.overflowing_sub(half).unwrap(), U128Fraction::zero());
        // 1/4 = 1/2 * 1/2
        let quarter = half.overflowing_mul(half).unwrap();
        println!("{}", quarter);
        // 1/4 = 1/2 - 1/4
        assert_eq!(quarter, half.overflowing_sub(quarter).unwrap());
        assert!((quarter.estimate_f64() - 0.25_f64).abs() < f64::EPSILON);
        // 1/4 < 1/2
        assert!(quarter < half);

        let x = (quarter * 7_u128).unwrap();
        println!("{}", x);
        assert!(x > half);
        assert!(half < x);

        // 7/4 = 1/4 + 3/2
        assert_eq!(
            x,
            quarter
                .overflowing_add(U128Fraction::new(3, 2).unwrap())
                .unwrap()
        );
        // 7/4 + 1 = 11/4
        assert_eq!((x + 1).unwrap(), U128Fraction::new(11, 4).unwrap());
        // 7/4 = 1, remainder = 3
        let (q, r) = x.quotient().unwrap();
        assert_eq!(q, 1);
        assert_eq!(r, 3);

        let max_v = U128Fraction::new(u128::MAX, 1).unwrap();

        assert!(max_v.overflowing_mul(max_v).is_err());
        assert!(max_v.overflowing_add(max_v).is_err());
        assert!((max_v * 2).is_err());
        assert!((max_v + 10).is_err());

        //Comparing always return false because of overflowing.
        assert!(
            !(U128Fraction::new(u128::MAX, 27).unwrap()
                < U128Fraction::new(u128::MAX, 5).unwrap())
        );

        assert!(
            !(U128Fraction::new(u128::MAX, 27).unwrap()
                 > U128Fraction::new(u128::MAX, 5).unwrap())
        );
    }
}
