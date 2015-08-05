#![feature(test)]
extern crate test;
extern crate unicode_segmentation;

use unicode_segmentation::UnicodeSegmentation;

use std::rc::Rc;

/// An immutable binary tree structure for storing and manipulating text.
///
/// [Ropes](https://en.wikipedia.org/wiki/Rope_%28data_structure%29) store text
/// as a binary tree of `Leaf` and `Concat` nodes, supporting O(log n)
/// implementations of most common operations (insert, delete, concat).  One
/// drawback is that indexing individual characters also becomes O(log n).
///
/// This particular implementation is built to operate on the level of Unicode
/// [grapheme clusters](http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries),
/// with the goal of making user level operations like "remove these three characters"
/// more natural than having to constantly perform O(n) searches to find the
/// cluster boundaries.  Because of this, all Rope operations that need an index
/// expect a valid grapheme index, instead of a byte or "character" index, unless
/// stated otherwise.

#[derive(Clone)]
pub enum Rope {
    Leaf { base: Rc<String>,
           start: usize,
           end: usize,
           graphemes: usize },
    Concat { left: Box<Rope>,
             right: Box<Rope>,
             graphemes: usize },
}

impl Rope {

    /// Create a new `Rope` from a given base.
    ///
    /// # Examples
    ///
    /// ```
    /// use rope::Rope;
    ///
    /// let rope = Rope::new("abc");
    /// assert_eq!(rope.to_string(), "abc");
    /// ```
    pub fn new<T: Into<String>>(base: T) -> Rope {
        let base: String = base.into();
        let len = base.len();
        let num_graphemes = count_grapheme_clusters(&base);
        Rope::Leaf { base: Rc::new(base),
                     start: 0,
                     end: len,
                     graphemes: num_graphemes }
    }

    /// Gets the number of Unicode grapheme clusters in a Rope.
    ///
    /// # Examples
    ///
    /// ```
    /// use rope::Rope;
    ///
    /// let s = "a̐éö̲"; // there are 11 bytes in this string, but only three individual characters.
    /// assert_eq!(s.len(), 11);
    ///
    /// let rope = Rope::new(s);
    /// assert_eq!(rope.num_graphemes(), 3);
    /// ```
    pub fn num_graphemes(&self) -> usize {
        match *self {
            Rope::Leaf { graphemes, .. } => graphemes,
            Rope::Concat { graphemes, .. } => graphemes,
        }
    }

    /// Divide a Rope into two at the nth (0-indexed) grapheme.
    /// Returns None if the grapheme index is 0 or out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use rope::Rope;
    ///
    /// let rope = Rope::new("foobar");
    /// let (left, right) = rope.split(3).unwrap();
    /// assert_eq!(left.to_string(), "foo");
    /// assert_eq!(right.to_string(), "bar");
    /// ```
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
                    Some((*left.clone(), *right.clone()))
                } else if left_graphemes > grapheme_index {
                    // split left into (l1, l2), return (l1, concat(l2, right))
                    left.split(grapheme_index).map(|(l1, l2)| {
                        (l1, Rope::join(l2, *right.clone()))
                    })
                } else {
                    // split right into (r1, r2), return (concat(left, r1), r2)
                    right.split(grapheme_index - left_graphemes).map(|(r1, r2)| {
                        (Rope::join(*left.clone(), r1), r2)
                    })
                }
            }
        }
    }

    /// Collect the pieces of this Rope into a continuous String.
    /// This will require a new memory allocation the same size as the entire Rope.
    ///
    /// # Examples
    ///
    /// ```
    /// use rope::Rope;
    ///
    /// let rope = Rope::new("abc");
    /// assert_eq!(rope.to_string(), String::from("abc"));
    /// ```
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

    /// Join two pieces into a Rope.
    ///
    /// # Examples
    ///
    /// ```
    /// use rope::Rope;
    ///
    /// let x = Rope::new("foo");
    /// let y = Rope::new("bar");
    /// let xy = Rope::join(x, y);
    /// assert_eq!(xy.to_string(), "foobar");
    /// ```
    pub fn join(left: Rope, right: Rope) -> Rope {
        let graphemes = left.num_graphemes() + right.num_graphemes();
        Rope::Concat { left: Box::new(left),
                       right: Box::new(right),
                       graphemes: graphemes }
    }

    /// Insert a string into a Rope, at the nth grapheme.
    /// Returns a new Rope, leaving the original unchanged.
    ///
    /// # Examples
    ///
    /// ```
    /// use rope::Rope;
    ///
    /// let x = Rope::new("foobaz");
    /// let y = x.insert(3, "bar").unwrap();
    /// assert_eq!(y.to_string(), "foobarbaz");
    /// ```
    pub fn insert<T: Into<String>>(&self, grapheme_index: usize, value: T) -> Option<Rope> {
        self.split(grapheme_index).map(|(left, right)| {
            let left = Rope::join(left, Rope::new(value));
            Rope::join(left, right)
        })
    }

    /// Convenience method to append a string to the Rope.
    /// Returns a new instance, leaving the original unchanged.
    ///
    /// This is equivalent to the following:
    /// ```
    /// let a = Rope::Join(a.clone(), Rope::new(b));
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// use rope::Rope;
    ///
    /// let x = Rope::new("foo");
    /// let y = x.append("bar");
    /// assert_eq!(y.to_string(), "foobar");
    /// ```
    pub fn append<T: Into<String>>(&self, value: T) -> Rope {
        Rope::join(self.clone(), Rope::new(value))
    }

    /// Remove a substring range from the Rope.
    /// The range begins with the nth grapheme, and continues for the specified number of graphemes.
    /// Returns None if the grapheme index is 0 or out of bounds.
    ///
    /// # Examples
    /// ```
    /// use rope::Rope;
    ///
    /// let x = Rope::new("foobarbaz");
    /// let y = x.delete(3, 3).unwrap();
    /// assert_eq!(y.to_string(), "foobaz");
    /// ```
    pub fn delete(&self, start_grapheme: usize, num_graphemes: usize) -> Option<Rope> {
        self.split(start_grapheme).map(|(head, middle)| {
            middle.split(num_graphemes).map(|(_, tail)| {
                Rope::join(head.clone(), tail.clone())
            })
        }).unwrap()
    }

    /// Return the balance ratio (left.num_graphemes() / right.num_graphemes()) for a given concat node.
    /// If this node is a leaf, returns 1.0.
    pub fn get_balance(&self) -> f32 {
        match *self {
            Rope::Leaf { .. } => 1.0,
            Rope::Concat { ref left, ref right, .. } => left.num_graphemes() as f32 / right.num_graphemes() as f32,
        }
    }

    /// Return a balanced version of the Rope, such that each concat node has a roughly equal number of
    /// graphemes in its left and right children.
    ///
    /// # Examples
    /// ```
    /// use rope::Rope;
    ///
    /// let mut rope = Rope::new("Grumpy wizards make ")
    ///     .append("toxic")
    ///     .append(" brew for ")
    ///     .append("the evil Queen")
    ///     .append(" and ")
    ///     .append("Jack")
    ///     .append(".");
    ///
    /// // This rope has become very lopsided, and will behave more like
    /// // a linked list than a binary tree.
    ///
    /// assert!(rope.get_balance() > 1.1 || rope.get_balance() < 0.9);
    ///
    /// // We can fix this by explicitly rebalancing the rope:
    /// rope = rope.balance();
    ///
    /// assert!(rope.get_balance() < 1.1 && rope.get_balance() > 0.9);
    /// ```
    pub fn balance(&self) -> Rope {
        let balance = self.get_balance();
        if balance > 1.1 || balance < 0.9 {
            let split = self.num_graphemes() / 2;
            if let Some((left, right)) = self.split(split) {
                return Rope::join(left.balance(), right.balance());
            }
        }
        self.clone()
    }

    /// Returns the nth grapheme as an Option<&str>, returns None if the index is out of bounds.
    ///
    /// # Examples
    /// ```
    /// use rope::Rope;
    ///
    /// let rope = Rope::new("a̐éö̲");
    /// assert_eq!(rope.get_nth_grapheme(1).unwrap(), "é");
    /// ```
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
    ///
    /// # Examples
    /// ```
    /// use rope::Rope;
    ///
    /// let rope = Rope::new("a̐éö̲");
    /// let mut iter = rope.graphemes();
    /// assert_eq!(iter.next(), Some("a̐"));
    /// assert_eq!(iter.next(), Some("é"));
    /// assert_eq!(iter.next(), Some("ö̲"));
    /// ```
    pub fn graphemes<'a>(&'a self) -> GraphemeIter<'a> {
        GraphemeIter { root: self, index: 0 }
    }
}

impl std::ops::Index<usize> for Rope {
    type Output = str;

    fn index<'a>(&'a self, _index: usize) -> &'a str {
        self.get_nth_grapheme(_index).expect("rope index out of bounds")
    }
}

/// An iterator over the graphemes of a Rope
/// Use `Rope::graphemes` to get an instance.
pub struct GraphemeIter<'a> {
    root: &'a Rope,
    index: usize,
}

// It would also be possible to implement this iterator with a stack, in order
// to avoid calling `Rope::get_nth_grapheme` for every iteration, which may or
// may not be more efficient.
impl <'a> std::iter::Iterator for GraphemeIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        let res = self.root.get_nth_grapheme(self.index);
        self.index += 1;
        res
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.root.num_graphemes() - self.index;
        (remaining, Some(remaining))
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

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    /// This is a simple utility for use during development,
    /// it simply formats a Rope in a way that exposes the inner concat/leaf structure
    #[allow(dead_code)]
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
                    v.extend( &str_parts(&left) );
                    v.push( ", " );
                    v.extend( &str_parts(&right) );
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

    fn create_rope_1k_flat() -> Rope {
        let base: String = ::std::iter::repeat("a").take(1000).collect();
        Rope::new(base)
    }

    fn create_rope_1k() -> Rope {
        create_rope_1k_flat().fixup_lengths(50, 100)
    }

    #[test]
    fn test_count_grapheme_clusters() {
        use ::count_grapheme_clusters;
        
        // 11 bytes, 3 grapheme clusters
        let s = "a̐éö̲";
        assert_eq!(s.len(), 11);
        assert_eq!(count_grapheme_clusters(s), 3);
    }

    #[test]
    fn test_grapheme_byte_index() {
        use ::grapheme_byte_index;
        
        let s = "a̐éö̲";
        assert_eq!(grapheme_byte_index(s, 0).unwrap(), 0);
        assert_eq!(grapheme_byte_index(s, 1).unwrap(), 3);
        assert_eq!(grapheme_byte_index(s, 2).unwrap(), 6);
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

        fn find_min_leaf(root: &Rope) -> usize {
            let len = root.num_graphemes();
            if let Rope::Concat { ref left, ref right, .. } = *root {
                ::std::cmp::min( find_min_leaf(&*left), find_min_leaf(&*right) )
            } else {
                len
            }
        }

        let rope = Rope::new("The quick brown fox jumps over the lazy dog.")
            .fixup_lengths(5, 10);
        assert_eq!(rope.num_graphemes(), 44);
        assert!(find_max_leaf(&rope) <= 10);
        assert!(find_min_leaf(&rope) >= 5);
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

    #[test]
    fn test_balance() {
        let mut rope = Rope::new("Grumpy wizards make ")
            .append("toxic")
            .append(" brew for ")
            .append("the evil Queen")
            .append(" and ")
            .append("Jack")
            .append(".");

        assert!(rope.get_balance() > 1.1 || rope.get_balance() < 0.9);

        rope = rope.balance();

        assert!(rope.get_balance() < 1.1 || rope.get_balance() > 0.9);
    }

    #[bench]
    fn bench_create_1000(b: &mut Bencher) {
        let base: String = ::std::iter::repeat("a").take(1000).collect();
        b.iter(|| {
            Rope::new(base.clone());
        });
    }

    #[bench]
    fn bench_fixup_1000(b: &mut Bencher) {
        let rope = create_rope_1k_flat();
        b.iter(|| {
            rope.fixup_lengths(100, 200);
        });
    }

    #[bench]
    fn bench_balance_1000(b: &mut Bencher) {
        let rope = create_rope_1k();
        b.iter(|| {
            rope.balance();
        });
    }

    #[bench]
    fn bench_index_middle(b: &mut Bencher) {
        let rope = create_rope_1k();
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
    fn bench_delete_1000(b: &mut Bencher) {
        let rope = create_rope_1k();
        b.iter(|| rope.delete(249, 499) );
    }

    #[bench]
    fn bench_insert(b: &mut Bencher) {
        let rope = Rope::new("foobaz");
        b.iter(|| rope.insert(3, "bar") );
    }

    #[bench]
    fn bench_insert_1000(b: &mut Bencher) {
        let rope = create_rope_1k();
        b.iter(|| rope.insert(819, "bar") );
    }

    #[bench]
    fn bench_to_string(b: &mut Bencher) {
        let rope = create_rope_1k();
        b.iter(|| rope.to_string() );
    }

    #[bench]
    fn bench_iter_1000(b: &mut Bencher) {
        let rope = create_rope_1k();
        b.iter(|| for _ in rope.graphemes() { });
    }
}
