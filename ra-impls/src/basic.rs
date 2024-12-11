use rand::Rng;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    X,
    Y,
    SinPi(Box<Expr>),
    CosPi(Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Avg(Box<Expr>, Box<Expr>),
    Thresh(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
}

pub fn interpret(e: &Expr, x: f64, y: f64) -> f64 {
    use std::f64::consts::PI;
    match e {
        Expr::X => x,
        Expr::Y => y,
        Expr::SinPi(e) => f64::sin(PI * interpret(e, x, y)),
        Expr::CosPi(e) => f64::cos(PI * interpret(e, x, y)),
        Expr::Mul(e1, e2) => interpret(e1, x, y) * interpret(e2, x, y),
        Expr::Avg(e1, e2) => (interpret(e1, x, y) + interpret(e2, x, y)) / 2.0,
        Expr::Thresh(e1, e2, e3, e4) => {
            if interpret(e1, x, y) < interpret(e2, x, y) {
                interpret(e3, x, y)
            } else {
                interpret(e4, x, y)
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
            2 => Expr::Mul(build(rng, d).into(), build(rng, d).into()),
            3 => Expr::Avg(build(rng, d).into(), build(rng, d).into()),
            _ => Expr::Thresh(
                build(rng, d).into(),
                build(rng, d).into(),
                build(rng, d).into(),
                build(rng, d).into(),
            ),
        }
    }
}
