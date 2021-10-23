use itertools::Itertools;
use pest::{
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator},
    Span,
};
use strum::IntoEnumIterator;

use crate::{
    ast::{Comment, InfixOp},
    grammar::{PairExt, Rule},
};

pub struct QCPairs<'source> {
    inner: Pairs<'source, Rule>,
    comments: Vec<Comment<'source>>,
}

impl<'source> QCPairs<'source> {
    pub fn new(pairs: Pairs<'source, Rule>) -> Self {
        QCPairs {
            inner: pairs,
            comments: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Option<QCPair<'source>> {
        let next = self.inner.next();
        match next {
            None => None,
            Some(pair) => match self.filter_non_comment(pair) {
                Some(pair) => Some(QCPair::new(pair)),
                None => self.next(),
            },
        }
    }

    pub fn peek(&mut self) -> Option<QCPair<'source>> {
        let next = self.inner.peek();
        match next {
            None => None,
            Some(pair) => match self.filter_non_comment(pair) {
                Some(pair) => Some(QCPair::new(pair)),
                None => {
                    // Consume the comment and try again
                    self.inner.next();
                    self.peek()
                }
            },
        }
    }

    pub fn try_get_next(&mut self, rule: Rule) -> Option<QCPair<'source>> {
        let next = self.inner.peek();

        match next {
            None => None,
            Some(pair) => {
                if pair.as_rule() == rule {
                    Some(QCPair::new(self.inner.next().unwrap()))
                } else {
                    None
                }
            }
        }
    }

    fn filter_non_comment(&mut self, pair: Pair<'source, Rule>) -> Option<Pair<'source, Rule>> {
        match pair.as_rule() {
            Rule::COMMENT => {
                let inner = pair.assert_and_unwrap_only_child(Rule::COMMENT);

                match inner.as_rule() {
                    Rule::line_comment => {
                        let content = inner
                            .assert_and_unwrap(Rule::line_comment)
                            .next()
                            .unwrap()
                            .as_str();
                        self.comments.push(Comment::Line(content));
                        None
                    }
                    Rule::block_comment => {
                        let mut inner = inner.assert_and_unwrap(Rule::block_comment);

                        let content = inner.next().unwrap().as_str();
                        let is_inline = inner.next().is_none();

                        self.comments.push(Comment::Block { content, is_inline });
                        None
                    }
                    _ => unreachable!(),
                }
            }
            _ => Some(pair),
        }
    }

    pub fn only_child(&mut self) -> QCPair<'source> {
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

    pub fn consume_all_comments(&mut self) {
        self.peek();
    }

    pub fn comments(&mut self) -> Vec<Comment<'source>> {
        std::mem::replace(&mut self.comments, Vec::new())
    }

    pub fn pest_iterator<'iter>(&'iter mut self) -> QCPairsPestIterator<'iter, 'source> {
        QCPairsPestIterator { inner: self }
    }
}

pub struct QCPairsPestIterator<'iter, 'source> {
    inner: &'iter mut QCPairs<'source>,
}

impl<'iter, 'source> QCPairsPestIterator<'iter, 'source> {
    pub fn new(inner: &'iter mut QCPairs<'source>) -> Self {
        QCPairsPestIterator { inner }
    }
}

impl<'iter, 'source> Iterator for QCPairsPestIterator<'iter, 'source> {
    type Item = Pair<'source, Rule>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|qc_pair| qc_pair.inner)
    }
}

impl<'source> Iterator for QCPairs<'source> {
    type Item = QCPair<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

pub struct QCPair<'source> {
    inner: Pair<'source, Rule>,
}

impl<'source> QCPair<'source> {
    pub fn new(pair: Pair<'source, Rule>) -> Self {
        QCPair { inner: pair }
    }

    pub fn children(self) -> QCPairs<'source> {
        QCPairs::new(self.inner.into_inner())
    }

    pub fn assert_rule(&self, rule: Rule) {
        self.inner.assert_rule(rule);
    }

    pub fn assert_and_unwrap_children(self, rule: Rule) -> QCPairs<'source> {
        self.assert_rule(rule);
        self.children()
    }

    pub fn as_rule(&self) -> Rule {
        self.inner.as_rule()
    }

    pub fn as_str(&self) -> &'source str {
        self.inner.as_str()
    }

    pub fn as_span(&self) -> Span<'source> {
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

// Generates the list of operators used by PrecClimber
pub fn get_operator_table() -> Vec<Operator<Rule>> {
    InfixOp::iter()
        .map(|op| {
            let assoc = if op.is_left_associative() {
                Assoc::Left
            } else {
                Assoc::Right
            };
            let operator = Operator::new(op.into_rule(), assoc);

            (op.precedence(), operator)
        })
        .into_group_map_by(|(precedence, _)| *precedence)
        .into_iter()
        .sorted_by_key(|(precedence, _v)| *precedence)
        .map(|(_, ops)| {
            ops.into_iter()
                .map(|(_, op)| op)
                .fold1(|l, r| l | r)
                .unwrap()
        })
        .collect_vec()
}
