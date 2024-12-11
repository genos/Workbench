use rand::Rng;

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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mul {
    e1: Expr,
    e2: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Avg {
    e1: Expr,
    e2: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Thresh {
    e1: Expr,
    e2: Expr,
    e3: Expr,
    e4: Expr,
}

pub fn interpret(e: &Expr, x: f64, y: f64) -> f64 {
    use std::f64::consts::PI;
    match e {
        Expr::X => x,
        Expr::Y => y,
        Expr::SinPi(e) => f64::sin(PI * interpret(e, x, y)),
        Expr::CosPi(e) => f64::cos(PI * interpret(e, x, y)),
        Expr::Mul(m) => interpret(&m.e1, x, y) * interpret(&m.e2, x, y),
        Expr::Avg(a) => (interpret(&a.e1, x, y) + interpret(&a.e2, x, y)) / 2.0,
        Expr::Thresh(t) => {
            if interpret(&t.e1, x, y) < interpret(&t.e2, x, y) {
                interpret(&t.e3, x, y)
            } else {
                interpret(&t.e4, x, y)
            }
        }
    }
}

pub fn build(rng: &mut impl Rng, depth: u32) -> Expr {
    if depth == 0 {
        if rng.gen() {
            Expr::X
        } else {
            Expr::Y
        }
    } else {
        let d = depth - 1;
        match rng.gen_range(0..5) {
            0 => Expr::SinPi(build(rng, d).into()),
            1 => Expr::CosPi(build(rng, d).into()),
            2 => Expr::Mul(
                Mul {
                    e1: build(rng, d),
                    e2: build(rng, d),
                }
                .into(),
            ),
            3 => Expr::Avg(
                Avg {
                    e1: build(rng, d),
                    e2: build(rng, d),
                }
                .into(),
            ),
            _ => Expr::Thresh(
                Thresh {
                    e1: build(rng, d),
                    e2: build(rng, d),
                    e3: build(rng, d),
                    e4: build(rng, d),
                }
                .into(),
            ),
        }
    }
}
