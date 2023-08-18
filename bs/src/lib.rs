use std::cmp::Ordering;

/// Searches the sorted `xs` for the given element `x`.
///
/// # Errors
///     If `x` isn't found, returns the index where it should be inserted.
pub fn binary_search<T: Ord>(x: &T, xs: &[T]) -> Result<usize, usize> {
    if let [] = xs {
        Err(0)
    } else {
        let i = xs.len() >> 1;
        match Ord::cmp(x, &xs[i]) {
            Ordering::Less => binary_search(x, &xs[..i]),
            Ordering::Equal => Ok(i),
            Ordering::Greater => binary_search(x, &xs[i + 1..])
                .map(|j| j + i + 1)
                .map_err(|j| j + i + 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use rstest::rstest;

    #[rstest]
    #[case(0, vec![0, 1, 2, 3], Ok(0))]
    #[case(1, vec![0, 1, 2, 3], Ok(1))]
    #[case(2, vec![0, 1, 2, 3], Ok(2))]
    #[case(3, vec![0, 1, 2, 3], Ok(3))]
    #[case(4, vec![0, 1, 2, 3], Err(4))]
    #[case(10, vec![], Err(0))]
    #[case(5, vec![5], Ok(0))]
    #[case(0, vec![1, 2], Err(0))]
    #[case(1, vec![1, 2], Ok(0))]
    #[case(2, vec![1, 2], Ok(1))]
    #[case(3, vec![1, 2], Err(2))]
    #[case(6, vec![1, 2, 3, 4, 5, 7], Err(5))]
    #[case(5, vec![1, 2, 3, 4, 5, 7], Ok(4))]
    fn it_works(#[case] x: usize, #[case] xs: Vec<usize>, #[case] expected: Result<usize, usize>) {
        assert_eq!(binary_search(&x, &xs), expected);
    }

    fn item_in_sorted_vec() -> impl Strategy<Value = (u32, Vec<u32>)> {
        prop::collection::vec(u32::MIN..=u32::MAX, 1..1000)
            .prop_flat_map(|mut xs| {
                xs.sort();
                let len = xs.len();
                (0..len, Just(xs))
            })
            .prop_map(|(i, xs)| {
                let n = xs[i];
                (n, xs)
            })
    }

    fn item_not_in_sorted_vec() -> impl Strategy<Value = (u32, Vec<u32>)> {
        prop::collection::vec(u32::MIN..=u32::MAX, 0..1000).prop_flat_map(|mut xs| {
            xs.sort();
            let ys = xs.clone();
            let n = (u32::MIN..=u32::MAX).prop_filter("filter out", move |k| !ys.contains(k));
            (n, Just(xs))
        })
    }

    proptest! {
        #[test]
        fn fuzz_empty(x: u32) {
            prop_assert!(binary_search(&x, &[]).is_err());
        }

        #[test]
        fn fuzz_contains((x, xs) in item_in_sorted_vec()) {
            prop_assert!(binary_search(&x, &xs).is_ok());
        }

        #[test]
        fn contains_matches_lib((x, xs) in item_in_sorted_vec()) {
            prop_assert_eq!(binary_search(&x, &xs), xs.binary_search(&x));
        }

        #[test]
        fn fuzz_no_contains((x, xs) in item_not_in_sorted_vec()) {
            prop_assert!(binary_search(&x, &xs).is_err());
        }

        #[test]
        fn no_contains_matches_lib((x, xs) in item_not_in_sorted_vec()) {
            prop_assert_eq!(binary_search(&x, &xs), xs.binary_search(&x));
        }
    }
}
