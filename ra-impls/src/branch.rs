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
pub struct Mul(Expr, Expr);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Avg(Expr, Expr);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Thresh(Expr, Expr, Expr, Expr);

pub fn interpret(e: &Expr, x: f64, y: f64) -> f64 {
    use std::f64::consts::PI;
    match e {
        Expr::X => x,
        Expr::Y => y,
        Expr::SinPi(e) => f64::sin(PI * interpret(e, x, y)),
        Expr::CosPi(e) => f64::cos(PI * interpret(e, x, y)),
        Expr::Mul(m) => interpret(&m.0, x, y) * interpret(&m.1, x, y),
        Expr::Avg(a) => f64::midpoint(interpret(&a.0, x, y), interpret(&a.1, x, y)),
        Expr::Thresh(t) => {
            if interpret(&t.0, x, y) < interpret(&t.1, x, y) {
                interpret(&t.2, x, y)
            } else {
                interpret(&t.3, x, y)
            }
        }
    }
}

pub fn build(rng: &mut impl Rng, depth: u32) -> Expr {
    if depth == 0 {
        if rng.random() { Expr::X } else { Expr::Y }
    } else {
        let d = depth - 1;
        match rng.random_range(0..5) {
            0 => Expr::SinPi(build(rng, d).into()),
            1 => Expr::CosPi(build(rng, d).into()),
            2 => Expr::Mul(Mul(build(rng, d), build(rng, d)).into()),
            3 => Expr::Avg(Avg(build(rng, d), build(rng, d)).into()),
            _ => Expr::Thresh(
                Thresh(build(rng, d), build(rng, d), build(rng, d), build(rng, d)).into(),
            ),
        }
    }
}
