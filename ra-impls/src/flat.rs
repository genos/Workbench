use rand::Rng;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct ExprRef(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Expr {
    X,
    Y,
    SinPi(ExprRef),
    CosPi(ExprRef),
    Mul(ExprRef, ExprRef),
    Avg(ExprRef, ExprRef),
    Thresh(ExprRef, ExprRef, ExprRef, ExprRef),
}

pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    fn with_capacity(n: u32) -> Self {
        Self(Vec::with_capacity(n as usize))
    }

    fn push(&mut self, e: Expr) -> ExprRef {
        let idx = self.0.len().try_into().expect("too many exprs in the pool");
        self.0.push(e);
        ExprRef(idx)
    }

    fn build(&mut self, rng: &mut impl Rng, depth: u32) -> ExprRef {
        if depth == 0 {
            if rng.gen() {
                self.push(Expr::X)
            } else {
                self.push(Expr::Y)
            }
        } else {
            let d = depth - 1;
            match rng.gen_range(0..5) {
                0 => {
                    let e = self.build(rng, d);
                    self.push(Expr::SinPi(e))
                }
                1 => {
                    let e = self.build(rng, d);
                    self.push(Expr::CosPi(e))
                }
                2 => {
                    let e1 = self.build(rng, d);
                    let e2 = self.build(rng, d);
                    self.push(Expr::Mul(e1, e2))
                }
                3 => {
                    let e1 = self.build(rng, d);
                    let e2 = self.build(rng, d);
                    self.push(Expr::Avg(e1, e2))
                }
                _ => {
                    let e1 = self.build(rng, d);
                    let e2 = self.build(rng, d);
                    let e3 = self.build(rng, d);
                    let e4 = self.build(rng, d);
                    self.push(Expr::Thresh(e1, e2, e3, e4))
                }
            }
        }
    }
}

pub fn interpret(pool: &ExprPool, x: f64, y: f64) -> f64 {
    use std::f64::consts::PI;
    let mut state = vec![0.0; pool.0.len()];
    for (i, expr) in pool.0.iter().enumerate() {
        state[i] = match expr {
            Expr::X => x,
            Expr::Y => y,
            Expr::SinPi(e) => f64::sin(PI * state[e.0 as usize]),
            Expr::CosPi(e) => f64::cos(PI * state[e.0 as usize]),
            Expr::Mul(e1, e2) => state[e1.0 as usize] * state[e2.0 as usize],
            Expr::Avg(e1, e2) => (state[e1.0 as usize] + state[e2.0 as usize]) / 2.0,
            Expr::Thresh(e1, e2, e3, e4) => {
                if state[e1.0 as usize] < state[e2.0 as usize] {
                    state[e3.0 as usize]
                } else {
                    state[e4.0 as usize]
                }
            }
        };
    }
    state[pool.0.len() - 1]
}

pub fn build(rng: &mut impl Rng, depth: u32) -> ExprPool {
    let mut pool = ExprPool::with_capacity((1 << depth) - 1);
    pool.build(rng, depth);
    pool
}
