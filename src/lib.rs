#![feature(test, vec_push_all)]

extern crate test;

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
    pub fn from_str(base: &str) -> Rope {
        let base = String::from(base);
        let num_graphemes = count_grapheme_clusters(&base);
        let len = base.len();
        Rope::Leaf { base: Rc::new(base),
                     start: 0,
                     end: len,
                     graphemes: num_graphemes }
    }
    /// Create a new rope leaf
    pub fn new<T: Into<String>>(base: T) -> Rope {
        let base: String = base.into();
        let len = base.len();
        let num_graphemes = count_grapheme_clusters(&base);
        Rope::Leaf { base: Rc::new(base),
                     start: 0,
                     end: len,
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
    /// Returns None if grapheme index is 0 or out of bounds
    pub fn split(&self, grapheme_index: usize) -> Option<(Rope, Rope)> {
        if grapheme_index == 0 { return None };
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
                let left_graphemes = left.num_graphemes();
                if left_graphemes == grapheme_index {
                    Some(((**left).clone(), (**right).clone()))
                } else if left_graphemes > grapheme_index {
                    // split left into (l1, l2), return (l1, concat(l2, right))
                    left.split(grapheme_index).map(|(l1, l2)| {
                        (l1, Rope::join_rc(Rc::new(l2), right.clone()))
                    })
                } else {
                    // split right into (r1, r2), return (concat(left, r1), r2)
                    right.split(grapheme_index - left_graphemes).map(|(r1, r2)| {
                        (Rope::join_rc(left.clone(), Rc::new(r1)), r2)
                    })
                }
            }
        }
    }
    /// Collect the pieces of this Rope into a continuous String
    pub fn to_string(&self) -> String {
        let mut buf = String::with_capacity(self.num_graphemes());

        fn append_to_string(root: &Rope, buf: &mut String) {
            match *root {
                Rope::Leaf { ref base, start, end, .. } => {
                    buf.push_str(&base[start..end]);
                },
                Rope::Concat { ref left, ref right, .. } => {
                    append_to_string(left, buf);
                    append_to_string(right, buf);
                },
            }
        }

        append_to_string(self, &mut buf);

        buf
    }
    /// Join two pieces into a Rope
    pub fn join(left: Rope, right: Rope) -> Rope {
        Rope::join_rc(Rc::new(left), Rc::new(right))
    }
    /// Join two Rc<Rope> into a Rope
    pub fn join_rc(left: Rc<Rope>, right: Rc<Rope>) -> Rope {
        let graphemes = left.num_graphemes() + right.num_graphemes();
        Rope::Concat { left: left,
                       right: right,
                       graphemes: graphemes }
    }
    /// Insert a string into a Rope by splitting and joining nodes
    /// Returns a new Rope, leaving the original unchanged
    pub fn insert(&self, grapheme_index: usize, value: &str) -> Option<Rope> {
        self.split(grapheme_index).map(|(left, right)| {
            let left = Rope::join(left, Rope::new(value));
            Rope::join(left, right)
        })
    }
    /// Append a string to the Rope, returning a new instance and leaving the original unchanged
    pub fn append(&self, value: &str) -> Rope {
        Rope::join(self.clone(), Rope::new(value))
    }
    /// Remove a substring range from the Rope
    pub fn delete(&self, start_grapheme: usize, num_graphemes: usize) -> Option<Rope> {
        self.split(start_grapheme).map(|(head, middle)| {
            middle.split(num_graphemes).map(|(_, tail)| {
                Rope::join(head.clone(), tail.clone())
            })
        }).unwrap()
    }
    /// Return the balance ratio (left.num_graphemes() / right.num_graphemes()) for a given concat node
    /// If this node is a leaf, returns 1.0
    pub fn get_balance(&self) -> f32 {
        match *self {
            Rope::Leaf { .. } => 1.0,
            Rope::Concat { ref left, ref right, .. } => left.num_graphemes() as f32 / right.num_graphemes() as f32,
        }
    }
    /// Returns the nth grapheme as an Option<&str>, returns None if the index is out of bounds
    pub fn get_nth_grapheme<'a>(&'a self, index: usize) -> Option<&'a str> {
        if index >= self.num_graphemes() {
            None
        } else {
            match *self {
                Rope::Leaf { ref base, start, end, .. } => {
                    nth_grapheme_cluster(&base[start..end], index)
                }
                Rope::Concat { ref left, ref right, .. } => {
                    if index < left.num_graphemes() { left.get_nth_grapheme(index) }
                    else { right.get_nth_grapheme(index - left.num_graphemes()) }
                }
            }
        }
    }
    /// Returns a new Rope where all neighboring nodes shorter than min_graphemes are merged and any
    /// nodes greater than max_graphemes are split
    /// Panics if max_graphemes is equal to 0, or if min_graphmes is greater than max_graphemes
    pub fn fixup_lengths(&self, min_graphemes: usize, max_graphemes: usize) -> Rope {
        assert!(max_graphemes > 0);
        assert!(min_graphemes <= max_graphemes);

        if self.is_leaf() && self.num_graphemes() > max_graphemes {
            // split long leafs
            if let Some((left, right)) = self.split(self.num_graphemes() / 2) {
                return Rope::join(left.fixup_lengths(min_graphemes, max_graphemes),
                                  right.fixup_lengths(min_graphemes, max_graphemes));
            }
        } else if !self.is_leaf() {
            if let Rope::Concat { ref left, ref right, graphemes } = *self {
                if graphemes <= min_graphemes {
                    // merge short concats
                    let mut merged = String::with_capacity(graphemes);
                    merged.push_str(&left.to_string());
                    merged.push_str(&right.to_string());
                    let len = merged.len();
                    return Rope::Leaf { base: Rc::new(merged), start: 0, end: len, graphemes: graphemes };
                } else {
                    // recurse
                    return Rope::join(left.fixup_lengths(min_graphemes, max_graphemes),
                                      right.fixup_lengths(min_graphemes, max_graphemes));
                }
            }
        }

        // nothing to be done
        return self.clone();
    }
    /// Returns true if this node is a Leaf
    fn is_leaf(&self) -> bool {
        match *self {
            Rope::Leaf { .. } => true,
            _ => false,
        }
    }
    /// Returns an iterator over the graphemes of this Rope
    pub fn graphemes<'a>(&'a self) -> GraphemeIter<'a> {
        GraphemeIter { root: self, index: 0 }
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
impl std::ops::Index<usize> for Rope {
    type Output = str;

    fn index<'a>(&'a self, _index: usize) -> &'a str {
        self.get_nth_grapheme(_index).expect("rope index out of bounds")
    }
}
pub struct GraphemeIter<'a> {
    root: &'a Rope,
    index: usize,
}
impl <'a> std::iter::Iterator for GraphemeIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        let res = self.root.get_nth_grapheme(self.index);
        self.index += 1;
        res
    }
}

impl std::convert::Into<Rope> for String {
    fn into(self) -> Rope {
        Rope::new(self)
    }
}

impl<'a> std::convert::Into<Rope> for &'a str {
    fn into(self) -> Rope {
        Rope::new(self)
    }
}

/// Count the number of unicode extended grapheme clusters in a string
/// This is O(n)
fn count_grapheme_clusters(s: &str) -> usize {
    UnicodeSegmentation::graphemes(s, true).count()
}

/// Find the byte index of the nth grapheme cluster in a string
/// Returns None if the grapheme index is out of bounds
fn grapheme_byte_index(base: &str, nth_grapheme: usize) -> Option<usize> {
    UnicodeSegmentation::grapheme_indices(base, true)
        .nth(nth_grapheme).map(|(i, _)| i)
}

/// Find the nth grapheme in a string
/// Returns None if the index is out of bounds
fn nth_grapheme_cluster(base: &str, nth_grapheme: usize) -> Option<&str> {
    UnicodeSegmentation::graphemes(base, true)
        .nth(nth_grapheme)
}
#[test]
fn test_count_grapheme_clusters() {
    // 11 bytes, 3 grapheme clusters
    let s = "a̐éö̲";
    assert_eq!(s.len(), 11);
    assert_eq!(count_grapheme_clusters(s), 3);
}

#[test]
fn test_grapheme_byte_index() {
    let s = "a̐éö̲";
    assert_eq!(grapheme_byte_index(s, 0).unwrap(), 0);
    assert_eq!(grapheme_byte_index(s, 1).unwrap(), 3);
    assert_eq!(grapheme_byte_index(s, 2).unwrap(), 6);
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[cfg(test)]
    fn to_string_debug(root: &Rope) -> String {
        let mut buf = String::with_capacity(root.num_graphemes());

        fn str_parts<'a>(root: &'a Rope) -> Vec<&'a str> {
            let mut v = vec![];
            match *root {
                Rope::Leaf { ref base, start, end, .. } => {
                    v.push( "<" );
                    v.push( &base[start..end] );
                    v.push( ">" );
                },
                Rope::Concat { ref left, ref right, .. } => {
                    v.push( "(" );
                    v.push_all(&str_parts(&left));
                    v.push( ", " );
                    v.push_all(&str_parts(&right));
                    v.push( ")" );
                },
            }
            v
        }

        for part in str_parts(root).iter() {
            buf.push_str(part);
        }

        buf
    }

    #[test]
    fn test_create_leaf() {
        let leaf = Rope::new("a̐éö̲");
        assert_eq!(leaf.num_graphemes(), 3);
        let leaf = Rope::new(String::from("abc"));
        assert_eq!(leaf.num_graphemes(), 3);
    }

    #[test]
    fn test_split_leaf() {
        let leaf = Rope::new("a̐éö̲a̐éö̲");
        let (left, right) = leaf.split(3).unwrap();
        assert_eq!(left.num_graphemes(), right.num_graphemes());
        assert_eq!(left.to_string(), "a̐éö̲");
    }

    #[test]
    fn test_create_rope() {
        let rope = Rope::new("The quick brown fox jumps over the lazy dog.");
        assert_eq!(rope.num_graphemes(), 44);
        assert_eq!(rope.to_string(), "The quick brown fox jumps over the lazy dog.");
    }

    #[test]
    fn test_fixup_lengths() {
        fn find_max_leaf(root: &Rope) -> usize {
            let len = root.num_graphemes();
            if let Rope::Concat { ref left, ref right, .. } = *root {
                ::std::cmp::max( find_max_leaf(&*left), find_max_leaf(&*right) )
            } else {
                len
            }
        }

        let rope = Rope::new("The quick brown fox jumps over the lazy dog.")
            .fixup_lengths(2, 4);
        assert_eq!(rope.num_graphemes(), 44);
        assert!(find_max_leaf(&rope) <= 4);
    }

    #[test]
    fn test_index() {
        let leaf = Rope::new("a̐éö̲a̐éö̲,a̐éö̲a̐éö̲ foo bar baz aéö").fixup_lengths(2, 4);
        assert_eq!(leaf.get_nth_grapheme(2).unwrap(), "ö̲");
        assert_eq!(&leaf[2], "ö̲");
    }

    #[test]
    fn test_iter() {
        let leaf = Rope::new("a̐éö̲a̐éö̲,a̐éö̲a̐éö̲ foo bar baz aéö").fixup_lengths(2, 4);
        let x: String = leaf.graphemes().collect();
        assert_eq!(leaf.to_string(), x);
    }

    #[test]
    fn test_join() {
        let left = Rope::new("foo");
        let right = Rope::new("bar");
        assert_eq!("foobar", Rope::join(left, right).to_string());
    }

    #[test]
    fn test_delete() {
        let rope = Rope::new("fooa̐éö̲bar").delete(3, 3).unwrap();
        assert_eq!("foobar", rope.to_string());
    }

    #[test]
    fn test_split_end() {
        let rope = Rope::new("abc");
        assert!(rope.split(3).is_none());
    }

    #[test]
    fn test_split_start() {
        let rope = Rope::new("abc");
        assert!(rope.split(0).is_none());
    }

    #[test]
    fn test_fraying() {
        let (a, b) = Rope::new("foo bar baz").split(4).expect("first split");
        let c = Rope::join(a, b);
        assert_eq!(c.to_string(), "foo bar baz");

        let (d, e) = c.split(4).expect("second split");
        let f = Rope::join(d, e);
        assert_eq!(f.to_string(), "foo bar baz");
    }

    #[bench]
    fn bench_create_1000(b: &mut Bencher) {
        let base: String = ::std::iter::repeat("a").take(1000).collect();
        b.iter(|| {
            Rope::from_str(&base);
        });
    }

    #[bench]
    fn bench_fixup_1000(b: &mut Bencher) {
        let base: String = ::std::iter::repeat("a").take(1000).collect();
        let rope = Rope::new(base);
        b.iter(|| {
            rope.fixup_lengths(100, 200);
        });
    }

    #[bench]
    fn bench_index_middle(b: &mut Bencher) {
        let base: String = ::std::iter::repeat("a").take(1000).collect();
        let rope = Rope::new(base).fixup_lengths(100, 200);
        b.iter(|| {
            assert_eq!(&rope[250], "a");
        });
    }

    #[bench]
    fn bench_clone(b: &mut Bencher) {
        let rope = Rope::new("foo");
        b.iter(|| rope.clone() )
    }

    #[bench]
    fn bench_join(b: &mut Bencher) {
        let left = Rope::new("foo");
        let right = Rope::new("bar");
        b.iter(|| Rope::join(left.clone(), right.clone()) );
    }

    #[bench]
    fn bench_delete(b: &mut Bencher) {
        let rope = Rope::new("foobarbaz");
        b.iter(|| rope.delete(3, 3) );
    }

    #[bench]
    fn bench_insert(b: &mut Bencher) {
        let rope = Rope::new("foobaz");
        b.iter(|| rope.insert(3, "bar") );
    }

    #[bench]
    fn bench_to_string(b: &mut Bencher) {
        let base: String = ::std::iter::repeat("a").take(1000).collect();
        let rope = Rope::new(base).fixup_lengths(50, 140);
        b.iter(|| rope.to_string() );
    }
}
