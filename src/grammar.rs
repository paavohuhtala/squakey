use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "quakec.pest"]
pub struct QuakeCParser;

pub trait PairExt<'a>
where
    Self: Sized + std::fmt::Debug,
{
    fn as_rule(&self) -> Rule;
    fn into_inner(self) -> Pairs<'a, Rule>;

    fn assert_and_unwrap(self, rule: Rule) -> Pairs<'a, Rule> {
        let as_rule = self.as_rule();
        if as_rule == rule {
            self.into_inner()
        } else {
            panic!("expected {:?}, found {:?}", rule, as_rule);
        }
    }

    fn assert_rule(&self, rule: Rule) {
        assert_eq!(self.as_rule(), rule);
    }

    fn assert_and_unwrap_only_child(self, rule: Rule) -> Pair<'a, Rule> {
        let mut unwrapped = self.assert_and_unwrap(rule);
        let first = unwrapped
            .next()
            .expect("Expected pair to contain at least one child.");

        match unwrapped.next() {
            Some(next) => {
                panic!("Expected exactly one child, found {:?}", next.as_rule());
            }
            None => first,
        }
    }
}

impl<'a> PairExt<'a> for Pair<'a, Rule> {
    fn as_rule(&self) -> Rule {
        self.as_rule()
    }

    fn into_inner(self) -> Pairs<'a, Rule> {
        Pair::into_inner(self)
    }
}
