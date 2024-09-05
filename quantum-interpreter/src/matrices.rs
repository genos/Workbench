use ndarray::{array, linalg::kron, Array1, Array2};
use num_complex::Complex64;
use std::sync::{Arc, LazyLock};

pub type Matrix = Array2<Complex64>;
pub type Vector = Array1<Complex64>;

static _1: LazyLock<Complex64> = LazyLock::new(|| Complex64::from(1.0));
static _0: LazyLock<Complex64> = LazyLock::new(|| Complex64::from(0.0));

pub static I: LazyLock<Arc<Matrix>> = LazyLock::new(|| Arc::new(Matrix::eye(2)));
pub static SWAP: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    Arc::new(array![
        [*_1, *_0, *_0, *_0],
        [*_0, *_0, *_1, *_0],
        [*_0, *_1, *_0, *_0],
        [*_0, *_0, *_0, *_1]
    ])
});
pub static H: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    let s = Complex64::from(std::f64::consts::FRAC_1_SQRT_2);
    Arc::new(array![[s, s], [s, -s]])
});
pub static CNOT: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    Arc::new(array![
        [*_1, *_0, *_0, *_0],
        [*_0, *_1, *_0, *_0],
        [*_0, *_0, *_0, *_1],
        [*_0, *_0, *_1, *_0]
    ])
});

pub fn kronecker_expt(u: &Matrix, n: usize) -> Matrix {
    match n {
        0 => array![[Complex64::from(1.0)]],
        1 => u.clone(),
        _ => kron(&kronecker_expt(u, n - 1), u),
    }
}

pub fn lift(u: &Matrix, i: usize, n: usize) -> Matrix {
    let left = kronecker_expt(&I, n - i - u.nrows().ilog2() as usize);
    let right = kronecker_expt(&I, i);
    kron(&left, &kron(u, &right))
}
