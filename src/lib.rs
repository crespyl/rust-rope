#![feature(collections,vec_push_all)]

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use std::rc::Rc;

const SPLIT_LEN: usize = 10;
const JOIN_LEN: usize = 5;
const REBALANCE_RATIO: f32 = 1.2;

struct Leaf {
    base: Rc<String>,
    start: usize,
    end: usize,
    graphemes: usize,
}
impl Leaf {
    /// Create a new Leaf from a String
    fn from_string(base: &str) -> Leaf {
        let base = String::from_str(base);
        let (start, end) = (0, base.len());
        Leaf::new(Rc::new(base), start, end)
    }
    /// Create a new Leaf
    /// This works like a Slice of the base String; base[start..end]
    fn new(base: Rc<String>, start: usize, end: usize) -> Leaf {
        let graphemes = UnicodeSegmentation::graphemes(&base[start..end], true).collect::<Vec<&str>>().len();
        Leaf { base: base, start: start, end: end, graphemes: graphemes }
    }
    /// Split a Leaf after the nth grapheme
    /// Panics if the grapheme index is out of range
    fn split(&self, grapheme_index: usize) -> (Rope, Rope) {
        let byte_index = UnicodeSegmentation::grapheme_indices(&self.base[self.start..self.end], true)
            .nth(grapheme_index).unwrap().0;
        let left = Leaf::new(self.base.clone(), self.start, self.start + byte_index);
        let right = Leaf::new(self.base.clone(), self.start + byte_index, self.end);
        (Rope::Leaf(left), Rope::Leaf(right))
    }
    /// Gets the number of Unicode grapheme clusters in this leaf
    fn num_graphemes(&self) -> usize {
        self.graphemes
    }
    /// Returns a &str of this Leafs contents
    fn to_str(&self) -> &str {
        &self.base[self.start..self.end]
    }
}
impl Clone for Leaf {
    fn clone(&self) -> Leaf {
        Leaf { base: self.base.clone(),
               start: self.start,
               end: self.end,
               graphemes: self.graphemes }
    }
}

struct Concat {
    left: Rope,
    right: Rope,
    graphemes: usize,
}
impl Concat {
    /// Join two Ropes
    fn new(left: &Rope, right: &Rope) -> Concat {
        let graphemes = left.num_graphemes() + right.num_graphemes();
        Concat { left: left.clone(), right: right.clone(), graphemes: graphemes }
    }

    /// Split a Rope after the nth grapheme
    fn split(&self, grapheme_index: usize) -> (Rope, Rope) {
        // if self is (a, b)
        // find out if the index is in the left or right child
        if self.left.num_graphemes() >= grapheme_index {
            // split a into (a1, a2), return ( a1, concat(a2, b) )
            let (a1, a2) = self.left.split(grapheme_index);
            return (a1, Rope::Concat(Rc::new(Concat::new(&a2, &self.right))));
        } else {
            // split b into (b1, b2), return ( concat(a, b1), b2 )
            let (b1, b2) = self.right.split(grapheme_index - self.left.num_graphemes());
            return (Rope::Concat(Rc::new(Concat::new(&self.left, &b1))), b2);
        }
    }
    
    /// Gets the number of Unicode grapheme clusters in the children of this node
    fn num_graphemes(&self) -> usize {
        self.graphemes
    }
}
impl Clone for Concat {
    fn clone(&self) -> Concat {
        Concat { left: self.left.clone(),
                 right: self.right.clone(),
                 graphemes: self.graphemes }
    }
}

enum Rope {
    Leaf(Leaf),
    Concat(Rc<Concat>),
}
impl Rope {
    /// Create a new Rope
    fn from_string(base: &str) -> Rope {
        Rope::Leaf(Leaf::from_string(base))
    }
    /// Gets the number of Unicode grapheme clusters in the children of this node
    fn num_graphemes(&self) -> usize {
        match *self {
            Rope::Leaf(ref leaf) => leaf.num_graphemes(),
            Rope::Concat(ref concat) => concat.num_graphemes(),
        }
    }
    /// Split a Rope after the nth grapheme
    fn split(&self, grapheme_index: usize) -> (Rope, Rope) {
        match *self {
            Rope::Leaf(ref leaf) => leaf.split(grapheme_index),
            Rope::Concat(ref concat) => concat.split(grapheme_index),
        }
    }
    /// Collect the pieces of this Rope into a continuous String
    fn to_string(&self) -> String {
        let mut buf = String::with_capacity(self.num_graphemes());

        fn str_parts<'a>(root: &'a Rope) -> Vec<&'a str> {
            let mut v = vec![];
            match *root {
                Rope::Leaf(ref leaf) => {
                    v.push(leaf.to_str());
                },
                Rope::Concat(ref concat) => {
                    v.push_all(&str_parts(&concat.left));
                    v.push_all(&str_parts(&concat.right));
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
    fn join(left: &Rope, right: &Rope) -> Rope {
        Rope::Concat(Rc::new(Concat::new(left, right)))
    }
    /// Insert a string into a Rope by splitting and joining nodes
    /// Returns a new Rope, leaving the original unchanged
    fn insert(&self, grapheme_index: usize, value: &str) -> Rope {
        let (left, right) = self.split(grapheme_index);
        let left = Rope::join(&left, &Rope::from_string(value));
        Rope::join(&left, &right)
    }
    
}
impl Clone for Rope {
    fn clone(&self) -> Rope {
        match *self {
            Rope::Leaf(ref leaf) => Rope::Leaf(leaf.clone()),
            Rope::Concat(ref concat) => Rope::Concat(concat.clone()),
        }
    }
}

/// Count the number of unicode extended grapheme clusters in a string
/// This is O(n)
fn count_clusters(s: &str) -> usize {
    UnicodeSegmentation::graphemes(s, true).collect::<Vec<&str>>().len()
}

#[test]
fn test_count_clusters() {
    // 11 bytes, 3 grapheme clusters
    let s = "a̐éö̲";
    assert_eq!(s.len(), 11);
    assert_eq!(count_clusters(s), 3);
}

#[test]
fn test_create_leaf() {
    let base = "a̐éö̲";
    let leaf = Leaf::from_string(base);
    assert_eq!(leaf.num_graphemes(), 3);
}

#[test]
fn test_split_leaf() {
    let base = "a̐éö̲a̐éö̲";
    let leaf = Leaf::from_string(base);
    let (left, right) = leaf.split(3);
    assert_eq!(left.num_graphemes(), right.num_graphemes());
    assert_eq!(left.to_string(), "a̐éö̲");
}

#[test]
fn test_create_rope() {
    let rope = Rope::from_string("The quick brown fox jumps over the lazy dog.");

    assert_eq!(rope.num_graphemes(), 44);
    assert_eq!(rope.to_string(), "The quick brown fox jumps over the lazy dog.");
}

#[test]
fn test_rope_operations() {
    let rope = Rope::from_string("The quick brown fox jumps over the lazy dog.");

    let (left, right) = rope.split(22);
    assert_eq!(left.to_string(), "The quick brown fox ju");
    assert_eq!(right.to_string(), "mps over the lazy dog.");

    let rope = Rope::join(&left, &Rope::from_string("mps over the tiny wooden fence!"));
    assert_eq!(rope.to_string(), "The quick brown fox jumps over the tiny wooden fence!");

    let rope2 = rope.insert(10, "(and really very clever) ");
    assert_eq!(rope2.to_string(), "The quick (and really very clever) brown fox jumps over the tiny wooden fence!");
}
