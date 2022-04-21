//! Time-traveling key-value store
#![forbid(missing_docs)]
#![forbid(unsafe_code)]

use std::collections::BTreeMap;
use std::time::Instant;

/// Time-traveling key-value store
pub struct Ttkv<K, V> {
    started: Instant,
    mapping: BTreeMap<u128, (K, V)>,
}

impl<K, V> Default for Ttkv<K, V> {
    fn default() -> Self {
        Self {
            started: Instant::now(),
            mapping: BTreeMap::default(),
        }
    }
}

impl<K, V> Ttkv<K, V> {
    /// Is this store empty?
    pub fn is_empty(&self) -> bool {
        self.mapping.is_empty()
    }
    /// Add a pair to the store. If a timestamp is specified, use it; otherwise, set the insertion
    /// timestamp to now - (when this store was created).
    pub fn put(&mut self, key: K, value: V, timestamp: Option<u128>) {
        let t = timestamp.unwrap_or_else(|| {
            Instant::now()
                .checked_duration_since(self.started)
                .unwrap_or_else(|| panic!("non-monotonic insertion"))
                .as_nanos()
        });
        self.mapping.insert(t, (key, value));
    }
    /// Grab a value associated with the given key, if it exists; if a timestamp is specified, grab the
    /// latest value inserted at or before the given timestamp.
    pub fn get(&self, key: &K, timestamp: Option<u128>) -> Option<&V>
    where
        K: PartialEq,
    {
        self.mapping
            .range(0..=timestamp.unwrap_or(u128::MAX))
            .filter(|(_, (k, _))| k == key)
            .last()
            .map(|(_, (_, v))| v)
    }
    /// The timestamps at which things were added to this store
    pub fn times(&self) -> Vec<u128> {
        self.mapping.keys().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn initially_empty() {
        let t = Ttkv::<String, String>::default();
        assert!(t.is_empty());
        assert!(t.times().is_empty());
    }

    proptest! {
        #[test]
        fn single_get(a: String, x: String) {
            let mut t = Ttkv::default();
            t.put(a.clone(), x.clone(), None);
            prop_assert_eq!(t.times().len(), 1);
            prop_assert_eq!(t.get(&a, None), Some(&x));
        }

        #[test]
        fn two_gets_different_keys(a: String, b: String, x: String, y: String) {
            prop_assume!(a != b);
            let mut t = Ttkv::default();
            t.put(a.clone(), x.clone(), None);
            t.put(b.clone(), y.clone(), None);
            prop_assert_eq!(t.times().len(), 2);
            prop_assert_eq!(t.get(&a, None), Some(&x));
            prop_assert_eq!(t.get(&b, None), Some(&y));
        }

        #[test]
        fn two_gets_same_key(a: String, x: String, y: String) {
            prop_assume!(x != y);
            let mut t = Ttkv::default();
            t.put(a.clone(), x, None);
            t.put(a.clone(), y.clone(), None);
            prop_assert_eq!(t.times().len(), 2);
            prop_assert_eq!(t.get(&a, None), Some(&y));
        }

        #[test]
        fn middle_get(a: String, x: String, y: String) {
            prop_assume!(x != y);
            let mut t = Ttkv::default();
            t.put(a.clone(), x.clone(), None);
            t.put(a.clone(), y, None);
            let times = t.times();
            prop_assert_eq!(times.len(), 2);
            let (t0, t1) = (times[0], times[1]);
            let delta = t1.saturating_sub(t0).saturating_div(2);
            prop_assert_eq!(t.get(&a, Some(t0 + delta)), Some(&x));
        }

        #[test]
        fn before_time(a: String, x: String, delta in (1u128 ..)) {
            let mut t = Ttkv::default();
            t.put(a.clone(), x, None);
            let times = t.times();
            prop_assert_eq!(times.len(), 1);
            prop_assert_eq!(t.get(&a, Some(times[0].saturating_sub(delta))), None);
        }
    }
}
