#![feature(vec_push_all)]

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use std::rc::Rc;

// const SPLIT_LEN: usize = 10;
// const JOIN_LEN: usize = 5;
// const REBALANCE_RATIO: f32 = 1.2;

pub enum Rope {
    Leaf { base: Rc<String>,
           start: usize,
           end: usize,
           graphemes: usize },
    Concat { left: Rc<Rope>,
             right: Rc<Rope>,
             graphemes: usize },
}
impl Rope {
    /// Create a new rope leaf
    pub fn from_str(base: &str) -> Rope {
        let num_graphemes = count_grapheme_clusters(base);
        Rope::Leaf { base: Rc::new(String::from(base)),
                     start: 0,
                     end: base.len(),
                     graphemes: num_graphemes }
    }
    /// Gets the number of Unicode grapheme clusters in the children of this node
    pub fn num_graphemes(&self) -> usize {
        match *self {
            Rope::Leaf { graphemes, .. } => graphemes,
            Rope::Concat { graphemes, .. } => graphemes,
        }
    }
    /// Split a Rope after the nth grapheme
    pub fn split(&self, grapheme_index: usize) -> Option<(Rope, Rope)> {
        match *self {
            Rope::Leaf { ref base, start, end, .. } => {
                grapheme_byte_index(&base[start..end], grapheme_index)
                    .map(|byte_index| {
                        let left = Rope::Leaf { base: base.clone(),
                                                start: start,
                                                end: start+byte_index,
                                                graphemes: count_grapheme_clusters(&base[start..start+byte_index]) };
                        let right = Rope::Leaf { base: base.clone(),
                                                 start: start+byte_index,
                                                 end: end,
                                                 graphemes: count_grapheme_clusters(&base[start+byte_index..end]) };
                        (left, right)
                    })
            },
            Rope::Concat{ ref left, ref right, .. } => {
                // self is (left, right)
                if left.num_graphemes() >= grapheme_index {
                    // split left into (l1, l2), return (l1, concat(l2, right))
                    left.split(grapheme_index).map(|(l1, l2)| {
                        (l1, Rope::Concat { left: Rc::new(l2.clone()),
                                            right: right.clone(),
                                            graphemes: l2.num_graphemes() + right.num_graphemes() })
                    })
                } else {
                    // split right into (r1, r2), return (concat(left, r1), r2)
                    right.split(grapheme_index - left.num_graphemes()).map(|(r1, r2)| {
                        (Rope::Concat { left: left.clone(),
                                        right: Rc::new(r1.clone()),
                                        graphemes: left.num_graphemes() + r1.num_graphemes() },
                         r2)
                    })
                }
            }
        }
    }
    /// Collect the pieces of this Rope into a continuous String
    pub fn to_string(&self) -> String {
        let mut buf = String::with_capacity(self.num_graphemes());

        fn str_parts<'a>(root: &'a Rope) -> Vec<&'a str> {
            let mut v = vec![];
            match *root {
                Rope::Leaf { ref base, start, end, .. } => {
                    v.push( &base[start..end] );
                },
                Rope::Concat { ref left, ref right, .. } => {
                    v.push_all(&str_parts(&left));
                    v.push_all(&str_parts(&right));
                },
            }
            v
        }
        for part in str_parts(self).iter() {
            buf.push_str(part);
        }
        
        buf
    }
    /// Join two pieces into a Rope
    pub fn join(left: Rope, right: Rope) -> Rope {
        let graphemes = left.num_graphemes() + right.num_graphemes();
        Rope::Concat { left: Rc::new(left),
                       right: Rc::new(right),
                       graphemes: graphemes }
    }
    /// Insert a string into a Rope by splitting and joining nodes
    /// Returns a new Rope, leaving the original unchanged
    pub fn insert(&self, grapheme_index: usize, value: &str) -> Option<Rope> {
        self.split(grapheme_index).map(|(left, right)| {
            let left = Rope::join(left, Rope::from_str(value));
            Rope::join(left, right)
        })
    }
    /// Append a string to the Rope, returning a new instance and leaving the original unchanged
    pub fn append(&self, value: &str) -> Rope {
        Rope::join(self.clone(), Rope::from_str(value))
    }
}
impl Clone for Rope {
    fn clone(&self) -> Rope {
        match *self {
            Rope::Leaf { ref base, start, end, graphemes } => Rope::Leaf { base: base.clone(),
                                                                           start: start,
                                                                           end: end,
                                                                           graphemes: graphemes },
            Rope::Concat { ref left, ref right, graphemes } => Rope::Concat { left: left.clone(),
                                                                              right: right.clone(),
                                                                              graphemes: graphemes },
        }
    }
}
/// Count the number of unicode extended grapheme clusters in a string
/// This is O(n)
fn count_grapheme_clusters(s: &str) -> usize {
    UnicodeSegmentation::graphemes(s, true).collect::<Vec<&str>>().len()
}

/// Find the byte index of the nth grapheme cluster in a string
/// Returns None if the grapheme index is out of bounds
fn grapheme_byte_index(base: &str, nth_grapheme: usize) -> Option<usize> {
    UnicodeSegmentation::grapheme_indices(base, true)
        .nth(nth_grapheme).map(|(i, _)| i)
}

#[test]
fn test_count_grapheme_clusters() {
    // 11 bytes, 3 grapheme clusters
    let s = "a̐éö̲";
    assert_eq!(s.len(), 11);
    assert_eq!(count_grapheme_clusters(s), 3);
}

#[test]
fn test_create_leaf() {
    let leaf = Rope::from_str("a̐éö̲");
    assert_eq!(leaf.num_graphemes(), 3);
}

#[test]
fn test_split_leaf() {
    let leaf = Rope::from_str("a̐éö̲a̐éö̲");
    let (left, right) = leaf.split(3).unwrap();
    assert_eq!(left.num_graphemes(), right.num_graphemes());
    assert_eq!(left.to_string(), "a̐éö̲");
}

#[test]
fn test_create_rope() {
    let rope = Rope::from_str("The quick brown fox jumps over the lazy dog.");
    assert_eq!(rope.num_graphemes(), 44);
    assert_eq!(rope.to_string(), "The quick brown fox jumps over the lazy dog.");
}

#[test]
fn test_rope_operations() {
    let rope = Rope::from_str("The quick brown fox jumps over the lazy dog.");

    let (left, right) = rope.split(22).unwrap();
    assert_eq!(left.to_string(), "The quick brown fox ju");
    assert_eq!(right.to_string(), "mps over the lazy dog.");

    let rope = Rope::join(left, Rope::from_str("mps over the tiny wooden fence!"));
    assert_eq!(rope.to_string(), "The quick brown fox jumps over the tiny wooden fence!");

    let rope2 = rope.insert(10, "(and really very clever) ").unwrap();
    assert_eq!(rope2.to_string(), "The quick (and really very clever) brown fox jumps over the tiny wooden fence!");

    let rope = rope.append(" One fish two fish, red fish blue fish.");
    assert_eq!(rope.to_string(), "The quick brown fox jumps over the tiny wooden fence! One fish two fish, red fish blue fish.");
}
