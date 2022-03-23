#![allow(dead_code)]
use crate::prelude::*;
use crate::triple_common_parser::TurtleValue;
#[derive(Debug, PartialEq)]
pub enum SparqlValue<'a> {
    Variable(&'a str),
    Block(Vec<SparqlValue<'a>>),
    TurtleValue(TurtleValue<'a>),
}

fn variable(s: &str) -> IResult<&str, SparqlValue> {
    map(
        preceded(tag("?").or(tag("$")), alphanumeric1),
        SparqlValue::Variable,
    )(s)
}

#[cfg(test)]
mod test {
    use crate::sparql::sparql_parser::{variable, SparqlValue};

    #[test]
    fn test_variable() {
        let s = "?pxx ";
        assert_eq!(SparqlValue::Variable("pxx"), variable(s).unwrap().1);
        let s = "$x ";
        assert_eq!(SparqlValue::Variable("x"), variable(s).unwrap().1);
    }
}
