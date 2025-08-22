use faer::{Col, Mat, c64, linalg::kron::kron, mat};
use std::sync::{Arc, LazyLock};

pub type Matrix = Mat<c64>;
pub type Vector = Col<c64>;

const _0: c64 = c64::ZERO;
const _1: c64 = c64::ONE;

pub static I: LazyLock<Arc<Matrix>> = LazyLock::new(|| Arc::new(Matrix::identity(2, 2)));
pub static SWAP: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    Arc::new(mat![
        [_1, _0, _0, _0],
        [_0, _0, _1, _0],
        [_0, _1, _0, _0],
        [_0, _0, _0, _1]
    ])
});
pub static H: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    let s = c64::from(std::f64::consts::FRAC_1_SQRT_2);
    Arc::new(mat![[s, s], [s, -s]])
});
pub static CNOT: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    Arc::new(mat![
        [_1, _0, _0, _0],
        [_0, _1, _0, _0],
        [_0, _0, _0, _1],
        [_0, _0, _1, _0]
    ])
});

pub fn kronecker_expt(u: &Matrix, n: usize) -> Matrix {
    // TODO possible optimization: fewer allocations
    let mut k = Mat::ones(1, 1);
    for _ in 0..n {
        let mut dst = Mat::zeros(k.nrows() * u.nrows(), k.ncols() * u.ncols());
        kron(dst.as_mut(), k.as_ref(), u.as_ref());
        k = dst;
    }
    k
}

pub fn lift(u: &Matrix, i: usize, n: usize) -> Matrix {
    let left = kronecker_expt(&I, n - i - u.nrows().ilog2() as usize);
    let right = kronecker_expt(&I, i);
    let mut first = Matrix::zeros(u.nrows() * right.nrows(), u.ncols() * right.ncols());
    kron(first.as_mut(), u.as_ref(), right.as_ref());
    let mut second = Matrix::zeros(left.nrows() * first.nrows(), left.ncols() * first.ncols());
    kron(second.as_mut(), left.as_ref(), first.as_ref());
    second
}
