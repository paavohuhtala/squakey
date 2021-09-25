use pest::{
    iterators::{Pair, Pairs},
    Span,
};

use crate::{
    ast::Comment,
    grammar::{PairExt, Rule},
};

pub struct QCPairs<'a> {
    inner: Pairs<'a, Rule>,
    comments: Vec<Comment<'a>>,
}

impl<'a> QCPairs<'a> {
    pub fn new(pairs: Pairs<'a, Rule>) -> Self {
        QCPairs {
            inner: pairs,
            comments: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Option<QCPair<'a>> {
        let next = self.inner.next();
        match next {
            None => None,
            Some(pair) => match self.consume_comment(pair) {
                Some(pair) => Some(QCPair::new(pair)),
                None => self.next(),
            },
        }
    }

    pub fn peek(&mut self) -> Option<QCPair<'a>> {
        let next = self.inner.peek();
        match next {
            None => None,
            Some(pair) => match self.consume_comment(pair) {
                Some(pair) => Some(QCPair::new(pair)),
                None => self.next(),
            },
        }
    }

    fn consume_comment(&mut self, pair: Pair<'a, Rule>) -> Option<Pair<'a, Rule>> {
        match pair.as_rule() {
            Rule::COMMENT => {
                let inner = pair.assert_and_unwrap_only_child(Rule::COMMENT);

                match inner.as_rule() {
                    Rule::line_comment => {
                        let content = inner
                            .assert_and_unwrap_only_child(Rule::line_comment)
                            .as_str();
                        self.comments.push(Comment::Line(content));
                        None
                    }
                    Rule::block_comment => {
                        let content = inner
                            .assert_and_unwrap_only_child(Rule::block_comment)
                            .as_str();
                        self.comments.push(Comment::Block(content));
                        None
                    }
                    _ => unreachable!(),
                }
            }
            _ => Some(pair),
        }
    }

    pub fn only_child(&mut self) -> QCPair<'a> {
        let next = self.next().expect("Expected exactly one child");
        let after = self.peek();

        match after {
            None => next,
            Some(after) => {
                panic!(
                    "Expected exactly one child, but found {:?} followed by {:?}",
                    next.inner, after.inner
                );
            }
        }
    }

    pub fn consume_to_end(&mut self) {
        while let Some(_) = self.next() {}
    }

    pub fn comments(&mut self) -> Vec<Comment<'a>> {
        std::mem::replace(&mut self.comments, Vec::new())
    }

    pub fn pest_iterator<'b>(&'b mut self) -> QCPairsPestIterator<'b, 'a> {
        QCPairsPestIterator { inner: self }
    }
}

pub struct QCPairsPestIterator<'a, 'b> {
    inner: &'a mut QCPairs<'b>,
}

impl<'a, 'b> QCPairsPestIterator<'a, 'b> {
    pub fn new(inner: &'a mut QCPairs<'b>) -> Self {
        QCPairsPestIterator { inner }
    }
}

impl<'a, 'b> Iterator for QCPairsPestIterator<'a, 'b> {
    type Item = Pair<'b, Rule>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|qc_pair| qc_pair.inner)
    }
}

impl<'a> Iterator for QCPairs<'a> {
    type Item = QCPair<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

pub struct QCPair<'a> {
    inner: Pair<'a, Rule>,
}

impl<'a> QCPair<'a> {
    pub fn new(pair: Pair<'a, Rule>) -> Self {
        QCPair { inner: pair }
    }

    pub fn children(self) -> QCPairs<'a> {
        QCPairs::new(self.inner.into_inner())
    }

    pub fn assert_rule(&self, rule: Rule) {
        self.inner.assert_rule(rule);
    }

    pub fn assert_and_unwrap_children(self, rule: Rule) -> QCPairs<'a> {
        self.assert_rule(rule);
        self.children()
    }

    pub fn as_rule(&self) -> Rule {
        self.inner.as_rule()
    }

    pub fn as_str(&self) -> &'a str {
        self.inner.as_str()
    }

    pub fn as_span(&self) -> Span<'a> {
        self.inner.as_span()
    }
}

impl std::fmt::Debug for QCPair<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("QCPair")
            .field("inner", &self.inner)
            .finish()
    }
}
