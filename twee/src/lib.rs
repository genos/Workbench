use std::collections::{HashMap, VecDeque};

/// Invariant: `Id`s are unique.
#[derive(Debug)]
pub struct Node<Id, T> {
    pub id: Id,
    pub datum: T,
    pub children: Vec<Node<Id, T>>,
}

impl<Id, T> Node<Id, T> {
    pub fn ids(&self) -> Vec<&Id> {
        let mut ids = Vec::new();
        for c in &self.children {
            ids.extend(c.ids());
        }
        ids.push(&self.id);
        ids
    }
}

/// A preprocessed lookup for searching which lives no longer than the original immutable tree.
pub struct Tome<'a, Id, T>(HashMap<&'a Id, (&'a Node<Id, T>, Vec<&'a Id>)>);

impl<'a, Id: Eq + std::hash::Hash, T> From<&'a Node<Id, T>> for Tome<'a, Id, T> {
    fn from(tree: &'a Node<Id, T>) -> Self {
        fn go<'b, I: Eq + std::hash::Hash, X>(
            node: &'b Node<I, X>,
            mapping: &mut HashMap<&'b I, (&'b Node<I, X>, Vec<&'b I>)>,
            ancestors: &mut VecDeque<&'b I>,
        ) {
            mapping.insert(
                &node.id,
                (
                    node,
                    // Compress ancestry to binary geometric indexing to save space.
                    // DO NOT change this without modifying `Tome::find_impl`!
                    ancestors
                        .iter()
                        .enumerate()
                        .filter_map(|(n, &i)| (n + 1).is_power_of_two().then_some(i))
                        .collect(),
                ),
            );
            // Depth-first exploration.
            ancestors.push_front(&node.id);
            for c in &node.children {
                go(c, mapping, ancestors);
                // Rewind for next child.
                while ancestors.pop_front_if(|id| **id != node.id).is_some() {}
            }
        }
        let mut mapping = HashMap::new();
        go(tree, &mut mapping, &mut VecDeque::new());
        Self(mapping)
    }
}

impl<'a, Id: Eq + std::hash::Hash, T> Tome<'a, Id, T> {
    pub fn find(&self, id: &'a Id, k: usize) -> Option<&Node<Id, T>> {
        self.find_impl(id, k, (), |()| ()).map(|(n, ())| n)
    }

    #[cfg(test)]
    fn find_count_calls(&self, id: &'a Id, k: usize) -> Option<(&Node<Id, T>, usize)> {
        self.find_impl(id, k, 0, |calls| calls.saturating_add(1))
    }

    fn find_impl<M>(
        &self,
        id: &'a Id,
        k: usize,
        metadata: M,
        update: impl Fn(M) -> M,
    ) -> Option<(&Node<Id, T>, M)> {
        if k == 0 {
            Some((self.0.get(id)?.0, update(metadata)))
        } else {
            // This relies on the binary geometric indexing from construction.
            // DO NOT change this without modifing the `From<&Node>` implementation!
            let z = k.trailing_zeros();
            self.find_impl(
                self.0.get(id)?.1.get(z as usize)?,
                k >> (z + 1),
                update(metadata),
                update,
            )
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashSet;
    use uuid::Uuid;

    fn arb_tree<T: Arbitrary + 'static>() -> impl Strategy<Value = Node<Uuid, T>> {
        // The `'static` bound is required by `proptest`'s `prop_recursive`.
        any::<T>()
            .prop_map(|datum| Node {
                id: Uuid::new_v4(),
                datum,
                children: Vec::new(),
            })
            .prop_recursive(32, 4096, 16, |inner| {
                (any::<T>(), prop::collection::vec(inner, 0..16)).prop_map(|(datum, children)| {
                    Node {
                        id: Uuid::new_v4(),
                        datum,
                        children,
                    }
                })
            })
    }

    proptest! {

        #[test]
        fn ids_are_unique(tree in arb_tree::<()>()) {
            let ids = tree.ids();
            let n = ids.len();
            let unique_ids = ids.iter().collect::<HashSet<_>>();
            prop_assert_eq!(n, unique_ids.len());
        }

        #[test]
        fn all_are_own_zeroth_parent(tree in arb_tree::<()>()) {
            let tome = Tome::from(&tree);
            for id in tree.ids() {
                prop_assert!(tome.find(id, 0).is_some_and(|n| n.id == *id));
            }
        }

        #[test]
        fn all_but_root_have_parent(tree in arb_tree::<()>()) {
            let tome = Tome::from(&tree);
            for id in tree.ids().into_iter().rev().skip(1) {
                prop_assert!(tome.find(id, 1).is_some());
            }
        }

        #[test]
        fn all_paths_bounded(tree in arb_tree::<()>()) {
            let ids = tree.ids();
            let n = ids.len();
            let bound = if n > 4 { 1 + n.ilog2() as usize } else { n };
            let tome = Tome::from(&tree);
            for id in ids {
                prop_assert!(tome.0[id].1.len() <= bound);
            }
        }

        #[test]
        fn search_length_bounded(tree in arb_tree::<()>()) {
            let tome = Tome::from(&tree);
            let ids = tree.ids();
            let n = ids.len();
            if n > 1 {
                for id in ids {
                    for k in 0..n {
                        let bound = 1 + if k == 0 { 0 } else { 1 + k.ilog2() as usize };
                        if let Some((_, calls)) = tome.find_count_calls(id, k) {
                            prop_assert!(calls <= bound);
                        }
                    }
                }
            }
        }

    }
}
