use ndarray::{array, linalg::kron, Array1, Array2};
use num_complex::Complex64;
use once_cell::sync::Lazy;
use std::sync::Arc;

pub type Matrix = Array2<Complex64>;
pub type Vector = Array1<Complex64>;

static _1: Lazy<Complex64> = Lazy::new(|| Complex64::from(1.0));
static _0: Lazy<Complex64> = Lazy::new(|| Complex64::from(0.0));

pub(crate) static I: Lazy<Arc<Matrix>> = Lazy::new(|| Arc::new(Matrix::eye(2)));
pub(crate) static SWAP: Lazy<Arc<Matrix>> = Lazy::new(|| {
    Arc::new(array![
        [*_1, *_0, *_0, *_0],
        [*_0, *_0, *_1, *_0],
        [*_0, *_1, *_0, *_0],
        [*_0, *_0, *_0, *_1]
    ])
});
pub(crate) static H: Lazy<Arc<Matrix>> = Lazy::new(|| {
    let s = Complex64::from(std::f64::consts::FRAC_1_SQRT_2);
    Arc::new(array![[s, s], [s, -s]])
});
pub(crate) static CNOT: Lazy<Arc<Matrix>> = Lazy::new(|| {
    Arc::new(array![
        [*_1, *_0, *_0, *_0],
        [*_0, *_1, *_0, *_0],
        [*_0, *_0, *_0, *_1],
        [*_0, *_0, *_1, *_0]
    ])
});

pub(crate) fn kronecker_expt(u: &Matrix, n: usize) -> Matrix {
    match n {
        0 => array![[Complex64::from(1.0)]],
        1 => u.clone(),
        _ => kron(&kronecker_expt(u, n - 1), u),
    }
}

pub(crate) fn lift(u: &Matrix, i: usize, n: usize) -> Matrix {
    let left = kronecker_expt(&I, n - i - u.nrows().ilog2() as usize);
    let right = kronecker_expt(&I, i);
    kron(&left, &kron(u, &right))
}
