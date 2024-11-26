use rand::Rng;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    X,
    Y,
    SinPi(Box<Expr>),
    CosPi(Box<Expr>),
    Mul(Box<Mul>),
    Avg(Box<Avg>),
    Thresh(Box<Thresh>),
}

pub trait Eval {
    fn eval(&self, x: f64, y: f64) -> f64;
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::X => f.write_str("x"),
            Expr::Y => f.write_str("y"),
            Expr::SinPi(e) => write!(f, "sin(pi*{e})"),
            Expr::CosPi(e) => write!(f, "cos(pi*{e})"),
            Expr::Mul(m) => f.write_str(&m.to_string()),
            Expr::Avg(a) => f.write_str(&a.to_string()),
            Expr::Thresh(t) => f.write_str(&t.to_string()),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, x: f64, y: f64) -> f64 {
        use std::f64::consts::PI;
        match self {
            Expr::X => x,
            Expr::Y => y,
            Expr::SinPi(e) => f64::sin(PI * e.eval(x, y)),
            Expr::CosPi(e) => f64::cos(PI * e.eval(x, y)),
            Expr::Mul(m) => m.eval(x, y),
            Expr::Avg(a) => a.eval(x, y),
            Expr::Thresh(t) => t.eval(x, y),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mul {
    e1: Expr,
    e2: Expr,
}

impl Eval for Mul {
    fn eval(&self, x: f64, y: f64) -> f64 {
        self.e1.eval(x, y) * self.e2.eval(x, y)
    }
}

impl fmt::Display for Mul {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}*{})", self.e1, self.e2)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Avg {
    e1: Expr,
    e2: Expr,
}

impl Eval for Avg {
    fn eval(&self, x: f64, y: f64) -> f64 {
        (self.e1.eval(x, y) + self.e2.eval(x, y)) / 2.0
    }
}

impl fmt::Display for Avg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(({}+{})/2)", self.e1, self.e2)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Thresh {
    e1: Expr,
    e2: Expr,
    e3: Expr,
    e4: Expr,
}

impl Eval for Thresh {
    fn eval(&self, x: f64, y: f64) -> f64 {
        if self.e1.eval(x, y) < self.e2.eval(x, y) {
            self.e3.eval(x, y)
        } else {
            self.e4.eval(x, y)
        }
    }
}

impl fmt::Display for Thresh {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}<{}?{}:{})", self.e1, self.e2, self.e3, self.e4)
    }
}

fn x() -> Expr {
    Expr::X
}

fn y() -> Expr {
    Expr::Y
}

fn sin_pi(e: Expr) -> Expr {
    Expr::SinPi(e.into())
}

fn cos_pi(e: Expr) -> Expr {
    Expr::CosPi(e.into())
}

fn mul(e1: Expr, e2: Expr) -> Expr {
    Expr::Mul(Mul { e1, e2 }.into())
}

fn avg(e1: Expr, e2: Expr) -> Expr {
    Expr::Avg(Avg { e1, e2 }.into())
}

fn thresh(e1: Expr, e2: Expr, e3: Expr, e4: Expr) -> Expr {
    Expr::Thresh(Thresh { e1, e2, e3, e4 }.into())
}

peg::parser! { grammar parser() for str {
    pub(super) rule expr() -> Expr = _x() / _y() / _sin_pi() / _cos_pi() / _mul() / _avg() / _thresh()
    rule _x() -> Expr = "x" { x() }
    rule _y() -> Expr = "y" { y() }
    rule _sin_pi() -> Expr = "sin(pi*" e:expr() ")" { sin_pi(e) }
    rule _cos_pi() -> Expr = "cos(pi*" e:expr() ")" { cos_pi(e) }
    rule _mul() -> Expr = "(" e1:expr() "*" e2:expr() ")" { mul(e1, e2) }
    rule _avg() -> Expr = "((" e1:expr() "+" e2:expr() ")/2)" { avg(e1, e2) }
    rule _thresh() -> Expr =
        "(" e1:expr() "<" e2:expr() "?" e3:expr() ":" e4:expr() ")" { thresh(e1, e2, e3, e4) }
    }
}

/// # Panics
/// If the parsing fails in any way
#[must_use]
pub fn parse(input: &str) -> Expr {
    match parser::expr(input) {
        Ok(expr) => expr,
        Err(err) => panic!("Unable to parse {err}"),
    }
}

pub fn build(rng: &mut impl Rng, depth: u64) -> Expr {
    if depth == 0 {
        if rng.gen() {
            x()
        } else {
            y()
        }
    } else {
        let d = depth - 1;
        match rng.gen_range(0..5) {
            0 => sin_pi(build(rng, d)),
            1 => cos_pi(build(rng, d)),
            2 => mul(build(rng, d), build(rng, d)),
            3 => avg(build(rng, d), build(rng, d)),
            _ => thresh(build(rng, d), build(rng, d), build(rng, d), build(rng, d)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;
    use std::mem::size_of_val;

    macro_rules! assert_close {
        ($left:expr, $right:expr) => {
            assert!(($left - $right).abs() < f64::EPSILON);
        };
    }

    #[test]
    fn to_string() {
        assert_eq!(
            thresh(x(), y(), x(), mul(sin_pi(x()), cos_pi(avg(x(), y())))).to_string(),
            "(x<y?x:(sin(pi*x)*cos(pi*((x+y)/2))))"
        );
    }

    #[test]
    fn evaluation() {
        assert_close!(sin_pi(avg(x(), y())).eval(0.5, -0.5), 0.0);
        assert_close!(
            sin_pi(avg(x(), y())).eval(0.3, 0.3),
            0.809_016_994_374_947_5
        );
        assert_close!(
            cos_pi(sin_pi(mul(
                cos_pi(avg(
                    cos_pi(x()),
                    mul(
                        cos_pi(cos_pi(avg(mul(y(), y()), cos_pi(x())))),
                        cos_pi(mul(sin_pi(cos_pi(y())), avg(sin_pi(x()), mul(x(), x()))))
                    )
                )),
                y()
            )))
            .eval(0.5, 0.2),
            0.118_612_572_814_630_63
        );
    }

    fn arb_expr() -> impl Strategy<Value = Expr> {
        prop_oneof!(Just(x()), Just(y())).prop_recursive(8, 256, 16, |leaf| {
            prop_oneof!(
                leaf.clone().prop_map(sin_pi),
                leaf.clone().prop_map(cos_pi),
                (leaf.clone(), leaf.clone()).prop_map(|(e1, e2)| mul(e1, e2)),
                (leaf.clone(), leaf.clone()).prop_map(|(e1, e2)| avg(e1, e2)),
                (leaf.clone(), leaf.clone(), leaf.clone(), leaf)
                    .prop_map(|(e1, e2, e3, e4)| thresh(e1, e2, e3, e4)),
            )
        })
    }

    proptest! {
        #[test]
        fn roundtrip(e in arb_expr()) {
            let s = e.to_string();
            let p = parse(&s);
            prop_assert_eq!(p, e);
        }

        #[test]
        fn size_of(e in arb_expr()) {
            assert_eq!(size_of_val(&e), 16);
        }
    }
}
