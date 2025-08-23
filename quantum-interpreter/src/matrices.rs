use faer::{Col, Mat, c64, linalg::kron::kron, mat};
use std::sync::{Arc, LazyLock};

pub type Matrix = Mat<c64>;
pub type Vector = Col<c64>;

const _0: c64 = c64::ZERO;
const _1: c64 = c64::ONE;
pub static I: LazyLock<Arc<Matrix>> = LazyLock::new(|| Matrix::identity(2, 2).into());
pub static SWAP: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    mat![
        [_1, _0, _0, _0],
        [_0, _0, _1, _0],
        [_0, _1, _0, _0],
        [_0, _0, _0, _1]
    ]
    .into()
});
pub static H: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    let s = c64::from(std::f64::consts::FRAC_1_SQRT_2);
    mat![[s, s], [s, -s]].into()
});
pub static CNOT: LazyLock<Arc<Matrix>> = LazyLock::new(|| {
    mat![
        [_1, _0, _0, _0],
        [_0, _1, _0, _0],
        [_0, _0, _0, _1],
        [_0, _0, _1, _0]
    ]
    .into()
});

pub fn kronecker_expt(u: &Matrix, exp: u32) -> Matrix {
    let mut k = Matrix::zeros(u.nrows().pow(exp), u.nrows().pow(exp));
    let mut tmp = k.clone();
    k[(0, 0)] = _1;
    let (mut r, mut c) = (1, 1);
    for _ in 0..exp {
        let (r_, c_) = (r, c);
        (r, c) = (r * u.nrows(), c * u.ncols());
        kron(
            tmp.submatrix_mut(0, 0, r, c),
            k.submatrix(0, 0, r_, c_),
            u.as_ref(),
        );
        k.copy_from(&tmp);
    }
    k
}

pub fn lift(u: &Matrix, i: u32, n: u32) -> Matrix {
    let left = kronecker_expt(&I, n - i - u.nrows().ilog2());
    let right = kronecker_expt(&I, i);
    let mut first = Matrix::zeros(u.nrows() * right.nrows(), u.ncols() * right.ncols());
    kron(first.as_mut(), u.as_ref(), right.as_ref());
    let mut second = Matrix::zeros(left.nrows() * first.nrows(), left.ncols() * first.ncols());
    kron(second.as_mut(), left.as_ref(), first.as_ref());
    second
}
