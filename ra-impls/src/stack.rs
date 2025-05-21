use rand::Rng;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Expr {
    X,
    Y,
    SinPi,
    CosPi,
    Mul,
    Avg,
    Thresh,
}

pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    fn with_capacity(n: u32) -> Self {
        Self(Vec::with_capacity(n as usize))
    }

    fn push(&mut self, e: Expr) {
        self.0.push(e);
    }

    fn build(&mut self, rng: &mut impl Rng, depth: u32) {
        if depth == 0 {
            if rng.random() {
                self.push(Expr::X);
            } else {
                self.push(Expr::Y);
            }
        } else {
            let d = depth - 1;
            match rng.random_range(0..5) {
                0 => {
                    self.build(rng, d);
                    self.push(Expr::SinPi);
                }
                1 => {
                    self.build(rng, d);
                    self.push(Expr::CosPi);
                }
                2 => {
                    self.build(rng, d);
                    self.build(rng, d);
                    self.push(Expr::Mul);
                }
                3 => {
                    self.build(rng, d);
                    self.build(rng, d);
                    self.push(Expr::Avg);
                }
                _ => {
                    self.build(rng, d);
                    self.build(rng, d);
                    self.build(rng, d);
                    self.build(rng, d);
                    self.push(Expr::Thresh);
                }
            }
        }
    }
}

#[allow(clippy::many_single_char_names)]
pub fn interpret(pool: &ExprPool, x: f64, y: f64) -> f64 {
    use std::f64::consts::PI;
    let mut stack = vec![];
    for e in &pool.0 {
        match e {
            Expr::X => stack.push(x),
            Expr::Y => stack.push(y),
            Expr::SinPi => {
                let z = stack.pop().expect("Empty stack");
                stack.push(f64::sin(PI * z));
            }
            Expr::CosPi => {
                let z = stack.pop().expect("Empty stack");
                stack.push(f64::cos(PI * z));
            }
            Expr::Mul => {
                let b = stack.pop().expect("Empty stack");
                let a = stack.pop().expect("Empty stack");
                stack.push(a * b);
            }
            Expr::Avg => {
                let b = stack.pop().expect("Empty stack");
                let a = stack.pop().expect("Empty stack");
                stack.push(f64::midpoint(a, b));
            }
            Expr::Thresh => {
                let d = stack.pop().expect("Empty stack");
                let c = stack.pop().expect("Empty stack");
                let b = stack.pop().expect("Empty stack");
                let a = stack.pop().expect("Empty stack");
                stack.push(if a < b { c } else { d });
            }
        }
    }
    stack.pop().expect("Empty stack")
}

pub fn build(rng: &mut impl Rng, depth: u32) -> ExprPool {
    let mut pool = ExprPool::with_capacity((1 << depth) - 1);
    pool.build(rng, depth);
    pool
}
