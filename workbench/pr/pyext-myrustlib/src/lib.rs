use pyo3::prelude::*;
use pyo3::wrap_pyfunction;
use rayon::prelude::*;

/// Counts matching consecutive pairs of bytes
fn _count_doubles(val: &str) -> u64 {
    let mut total = 0u64;
    let mut chars = val.bytes();
    if let Some(mut a) = chars.next() {
        for b in chars {
            if a == b {
                total += 1;
            }
            a = b;
        }
    }
    total
}

/// Counts matching consecutive pairs of bytes
#[pyfunction]
fn count_doubles(val: &str) -> PyResult<u64> {
    Ok(_count_doubles(val))
}

/// Counts multiple doubles
#[pyfunction]
fn count_mult_doubles(vals: Vec<&str>) -> PyResult<u64> {
    Ok(vals.par_iter().map(|&val| _count_doubles(val)).sum())
}

/// This is a Python module implemented in Rust
#[pymodule]
fn myrustlib(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(count_doubles))?;
    m.add_wrapped(wrap_pyfunction!(count_mult_doubles))?;
    Ok(())
}
