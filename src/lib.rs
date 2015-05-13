extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use std::rc::Rc;

const SPLIT_LEN: usize = 10;
const JOIN_LEN: usize = 5;
const REBALANCE_RATIO: f32 = 1.2;

/// A "base" segment of a Rope.
/// Usually a String, but could in principle be a large file buffer or similar.
trait RopeSegment {
    /// Returns the number of unicode grapheme clusters in this segment
    fn grapheme_len(&self) -> usize;
    /// Return Rc pointers to segments before and after the grapheme cluster specified by the `index` parameter
    fn split(&self, index: usize) -> (Rc<RopeSegment>, Rc<RopeSegment>);
    
}

enum RopeNode {
    Leaf(String),
    Node { len: usize, left: Rc<RopeNode>, right: Rc<RopeNode> },
}

impl RopeNode {    
    pub fn new_leaf(value: String) -> RopeNode {
        RopeNode::Leaf(value).adjust()
    }
    pub fn push_str(self, value: String) -> RopeNode {
        let l = self.len() + value.len();
        RopeNode::Node { len: l, left: Rc::new(self), right: Rc::new(RopeNode::new_leaf(value)) }
    }
    fn split(&self, index: usize) -> (RopeNode, RopeNode) {
        use RopeNode::*;

        match *self {
            Leaf(ref s) => {
                let len = s.len();
                let left = s[..len].to_string();
                let right = s[len..].to_string();
                (RopeNode::new_leaf(left), RopeNode::new_leaf(right))
            },
            Node { len, ref left, ref right } => {
                let left_len = left.len();
                let right_len = right.len();
                if index < left_len {
                    let (a, b) = left.split(index);
                    (a, RopeNode::join(Rc::new(b), right.clone()))
                } else {
                    let (b, c) = right.split(index-left_len);
                    (RopeNode::join(left.clone(), Rc::new(b)), c)
                }
            }
        }
        
    }
    fn join(left: Rc<RopeNode>, right: Rc<RopeNode>) -> RopeNode {
        RopeNode::Node { len: left.len() + right.len(), left: left.clone(), right: right.clone() }
    }
    fn adjust(self) -> RopeNode {
        use RopeNode::*;

        let l = self.len();
        match self {
            Leaf(s) => {
                if l > SPLIT_LEN {
                    let split = l / 2;
                    let left = s[0..split].to_string();
                    let right = s[split..].to_string();
                    Node { len: l,
                           left: Rc::new(RopeNode::new_leaf(left).adjust()),
                           right: Rc::new(RopeNode::new_leaf(right).adjust()) }
                } else {
                    Leaf(s)
                }
            },
            Node { len, left, right } => {
                if len < JOIN_LEN {
                    let mut s = String::with_capacity(len);
                    left.append_to(&mut s);
                    right.append_to(&mut s);
                    Leaf(s)
                } else {
                    Node { len: len, left: left, right: right }
                }
            },
        }
    }
    fn append_to(&self, buffer: &mut String) {
        match *self {
            RopeNode::Leaf(ref s) => {
                buffer.push_str(s);
            },
            RopeNode::Node { len, ref left, ref right } => {
                left.append_to(buffer);
                right.append_to(buffer);
            }
        }
    }
    pub fn len(&self) -> usize {
        match *self {
            RopeNode::Leaf(ref s) => s.len(),
            RopeNode::Node{ len, .. } => len
        }
    }
    pub fn to_string(&self) -> String {
        let mut buffer = String::with_capacity(self.len());
        self.append_to(&mut buffer);
        buffer
    }
}

fn walk(depth: usize, rope: &RopeNode) {
    for i in 0..depth*2 {
        print!(" ");
    }
    match *rope {
        RopeNode::Leaf(ref s) => println!(" Leaf ({}): {:?}", s.len(), s),
        RopeNode::Node { len, ref left, ref right } => {
            println!(" Node ({}):", len);
            walk(depth+1, &*left);
            walk(depth+1, &*right);
        }
    }
}

#[test]
fn it_works() {
    let base = "the quick brown fox jumped over the lazy dog.";

    let rope = RopeNode::new_leaf(base.to_string());
    assert_eq!(rope.to_string(), base.to_string());

    println!("---");
    println!("{}", rope.to_string());
    walk(0, &rope);
    
    let rope = rope.adjust();
    println!("---");
    println!("{}", rope.to_string());
    walk(0, &rope);

    assert_eq!(rope.to_string(), base.to_string());

    let rope = rope.push_str("  Foo bar baz baz bar foo.".to_string());
    println!("---");
    println!("{}", rope.to_string());
    walk(0, &rope);

}
