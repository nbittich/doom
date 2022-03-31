use crate::prelude::*;
use crate::sparql::built_in::built_in_call;
use crate::sparql::common::var;
use crate::sparql::path::{path as common_path, Path};
use crate::triple_common_parser::literal::literal_sparql as common_literal;
use crate::triple_common_parser::{
    paren_close, paren_open, tag_no_case_no_space, tag_no_space, Literal,
};
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RelationalOperator {
    Equals,
    Diff,
    Lower,
    Greater,
    LowerOrEqual,
    GreaterOrEqual,
    In,
    NotIn,
}
#[derive(Debug, PartialEq)]
pub enum BuiltInCall<'a> {
    Str(Expr<'a>),
    Lang(Expr<'a>),
    LangMatches {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    DataType(Expr<'a>),
    Bound(Expr<'a>),
    Iri(Expr<'a>),
    BNode(Option<Expr<'a>>),
    Rand,
    Abs(Expr<'a>),
    Ceil(Expr<'a>),
    Floor(Expr<'a>),
    Round(Expr<'a>),
    Concat(Expr<'a>),
    SubStr,
    StrLen(Expr<'a>),
    Replace,
    UCase(Expr<'a>),
    LCase(Expr<'a>),
    EncodeForUri(Expr<'a>),
    Contains {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    StrStarts {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    StrEnds {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    StrBefore {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    StrAfter {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    Year(Expr<'a>),
    Month(Expr<'a>),
    Day(Expr<'a>),
    Hours(Expr<'a>),
    Minutes(Expr<'a>),
    Seconds(Expr<'a>),
    Timezone(Expr<'a>),
    Tz(Expr<'a>),
    Now,
    Uuid,
    StrUuid,
    MD5(Expr<'a>),
    Sha1(Expr<'a>),
    Sha256(Expr<'a>),
    Sha384(Expr<'a>),
    Sha512(Expr<'a>),
    Coalesce(Expr<'a>),
    If {
        first: Expr<'a>,
        second: Expr<'a>,
        third: Expr<'a>,
    },
    StrLang {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    StrDt {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    SameTerm {
        left: Expr<'a>,
        right: Expr<'a>,
    },
    IsIri(Expr<'a>),
    IsBlank(Expr<'a>),
    IsLiteral(Expr<'a>),
    IsNumeric(Expr<'a>),
    // TODO
    Regex,
    Exists,
    NotExists,
    Count,
    Sum,
    Min,
    Max,
    Avg,
    Sample,
    GroupConcat,
}
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Bracketed(Box<Expr<'a>>),
    ConditionalOr {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    ConditionalAnd {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    AsExpr {
        expression: Box<Expr<'a>>,
        variable: Box<Expr<'a>>,
    },
    Relational {
        left: Box<Expr<'a>>,
        operator: RelationalOperator,
        right: Box<Expr<'a>>,
    },
    List(VecDeque<Expr<'a>>),
    Arithmetic {
        left: Box<Expr<'a>>,
        operator: ArithmeticOperator,
        right: Box<Expr<'a>>,
    },
    BuiltInCall(Box<BuiltInCall<'a>>),
    Literal(Literal<'a>),
    Path(Path<'a>),
    Variable(&'a str),
}
pub(crate) fn as_expr(s: &str) -> ParserResult<Expr> {
    preceded(
        paren_open,
        terminated(
            map(
                separated_pair(expr, tag_no_case_no_space("AS"), variable),
                |(expression, variable)| Expr::AsExpr {
                    expression: Box::new(expression),
                    variable: Box::new(variable),
                },
            ),
            paren_close,
        ),
    )(s)
}
pub(super) fn bracketed(s: &str) -> ParserResult<Expr> {
    preceded(
        paren_open,
        map(terminated(expr, paren_close), |ex| {
            Expr::Bracketed(Box::new(ex))
        }),
    )(s)
}
pub(super) fn list(s: &str) -> ParserResult<Expr> {
    preceded(
        paren_open,
        terminated(
            map(
                separated_list1(char(','), alt((built_in_call, variable, literal, path))),
                |list| Expr::List(VecDeque::from(list)),
            ),
            paren_close,
        ),
    )(s)
}
fn relational<'a>(s: &'a str) -> ParserResult<Expr<'a>> {
    let make_rel = |op: RelationalOperator| {
        move |(left, right)| Expr::Relational {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        }
    };
    alt((
        map(
            separated_pair(additive, tag_no_space("<="), additive),
            make_rel(RelationalOperator::LowerOrEqual),
        ),
        map(
            separated_pair(additive, tag_no_space(">="), additive),
            make_rel(RelationalOperator::GreaterOrEqual),
        ),
        map(
            separated_pair(additive, tag_no_space("<"), additive),
            make_rel(RelationalOperator::Lower),
        ),
        map(
            separated_pair(additive, tag_no_space(">"), additive),
            make_rel(RelationalOperator::Greater),
        ),
        map(
            separated_pair(additive, tag_no_space("!="), additive),
            make_rel(RelationalOperator::Diff),
        ),
        map(
            separated_pair(additive, tag_no_space("="), additive),
            make_rel(RelationalOperator::Equals),
        ),
        map(
            separated_pair(additive, tag_no_case_no_space("NOT IN"), list),
            make_rel(RelationalOperator::NotIn),
        ),
        map(
            separated_pair(additive, tag_no_case_no_space("IN"), list),
            make_rel(RelationalOperator::In),
        ),
    ))(s)
}
pub(super) fn additive(s: &str) -> ParserResult<Expr> {
    alt((built_in_call, bracketed, literal, path, variable))(s)
}
fn arithmetic<'a>(s: &'a str) -> ParserResult<Expr<'a>> {
    let make_rel = |op| {
        move |(left, right)| Expr::Arithmetic {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        }
    };

    alt((
        map(
            separated_pair(additive, tag_no_space("+"), additive),
            make_rel(ArithmeticOperator::Add),
        ),
        map(
            separated_pair(additive, tag_no_space("-"), additive),
            make_rel(ArithmeticOperator::Subtract),
        ),
        map(
            separated_pair(additive, tag_no_space("/"), additive),
            make_rel(ArithmeticOperator::Divide),
        ),
        map(
            separated_pair(additive, tag_no_space("*"), additive),
            make_rel(ArithmeticOperator::Multiply),
        ),
    ))(s)
}
fn conditional(s: &str) -> ParserResult<Expr> {
    alt((
        map(
            separated_pair(relational, tag_no_space("||"), relational),
            |(left, right)| Expr::ConditionalOr {
                left: Box::new(left),
                right: Box::new(right),
            },
        ),
        map(
            separated_pair(relational, tag_no_space("&&"), relational),
            |(left, right)| Expr::ConditionalAnd {
                left: Box::new(left),
                right: Box::new(right),
            },
        ),
    ))(s)
}
pub(super) fn literal(s: &str) -> ParserResult<Expr> {
    map(common_literal, Expr::Literal)(s)
}
pub(super) fn path(s: &str) -> ParserResult<Expr> {
    map(common_path, Expr::Path)(s)
}
pub(super) fn variable(s: &str) -> ParserResult<Expr> {
    map(var, Expr::Variable)(s)
}

pub(crate) fn expr(s: &str) -> ParserResult<Expr> {
    alt((
        bracketed,
        conditional,
        relational,
        arithmetic,
        built_in_call,
        variable,
        path,
        literal,
    ))(s)
}

#[cfg(test)]
mod test {
    use crate::sparql::expression::expr;
    use crate::sparql::expression::ArithmeticOperator::Multiply;
    use crate::sparql::expression::BuiltInCall::{IsIri, Str};
    use crate::sparql::expression::Expr;
    use crate::sparql::expression::Expr::{
        Arithmetic, Bracketed, BuiltInCall, List, Relational, Variable,
    };
    use crate::sparql::expression::RelationalOperator;
    use crate::sparql::expression::RelationalOperator::{Diff, Equals, NotIn};
    use crate::sparql::path::Path;
    use crate::triple_common_parser::Iri::Enclosed;
    use crate::triple_common_parser::Literal;
    use std::collections::VecDeque;
    macro_rules! a_box {
        ($a:expr) => {
            Box::new($a)
        };
    }
    #[test]
    fn test_expr() {
        let s = "(?price < 30)";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            Bracketed(a_box!(Relational {
                left: a_box!(Expr::Variable("price")),
                operator: RelationalOperator::Lower,
                right: a_box!(Expr::Literal(Literal::Integer(30))),
            })),
            exp
        );
        let s = "(?n = ?m)";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            Bracketed(a_box!(Relational {
                left: a_box!(Expr::Variable("n")),
                operator: RelationalOperator::Equals,
                right: a_box!(Expr::Variable("m")),
            })),
            exp
        );
        let s = "?n > ?m";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            Relational {
                left: a_box!(Expr::Variable("n")),
                operator: RelationalOperator::Greater,
                right: a_box!(Expr::Variable("m")),
            },
            exp
        );
        let s = "?x != ?y";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            Relational {
                left: a_box!(Expr::Variable("x")),
                operator: RelationalOperator::Diff,
                right: a_box!(Expr::Variable("y")),
            },
            exp
        );

        let s = "?x * ?y";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            exp,
            Arithmetic {
                left: a_box!(Expr::Variable("x",)),
                operator: Multiply,
                right: a_box!(Expr::Variable("y",)),
            }
        );
        let s = "?x IN(?s, ?p, <http://xx.com>)";
        let (_, exp) = expr(s).unwrap();

        assert_eq!(
            exp,
            Relational {
                left: a_box!(Expr::Variable("x",)),
                operator: RelationalOperator::In,
                right: a_box!(Expr::List(VecDeque::from(vec![
                    Expr::Variable("s",),
                    Expr::Variable("p",),
                    Expr::Path(Path::Iri(Enclosed("http://xx.com",),),),
                ]),))
            }
        );
        let s = "(?x NOT IN (?s, ?p, <http://xx.com>))";
        let (_, exp) = expr(s).unwrap();

        assert_eq!(
            exp,
            Bracketed(a_box!(Relational {
                left: a_box!(Expr::Variable("x",)),
                operator: RelationalOperator::NotIn,
                right: a_box!(Expr::List(VecDeque::from(vec![
                    Expr::Variable("s",),
                    Expr::Variable("p",),
                    Expr::Path(Path::Iri(Enclosed("http://xx.com",),),),
                ]),))
            }))
        );
    }

    #[test]
    fn test_built_in() {
        let s = "(STR(?s) NOT IN (?s, ?p, ?o))";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            Bracketed(a_box!(Relational {
                left: a_box!(BuiltInCall(a_box!(Str(Variable("s"))))),
                operator: NotIn,
                right: a_box!(List(VecDeque::from(vec![
                    Variable("s",),
                    Variable("p",),
                    Variable("o",),
                ]),)),
            })),
            exp
        );
        let s = "(STR(?s) = <http://xxx.com/s>)";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            Bracketed(a_box!(Relational {
                left: a_box!(BuiltInCall(a_box!(Str(Variable("s"))))),
                operator: Equals,
                right: a_box!(Expr::Path(Path::Iri(Enclosed("http://xxx.com/s")))),
            })),
            exp
        );
        let s = "isIri(<http://xxx.com/s>) != TRUE";
        let (_, exp) = expr(s).unwrap();
        assert_eq!(
            exp,
            Relational {
                left: a_box!(BuiltInCall(a_box!(IsIri(Expr::Path(Path::Iri(Enclosed(
                    "http://xxx.com/s"
                ))))))),
                operator: Diff,
                right: a_box!(Expr::Literal(Literal::Boolean(true))),
            }
        )
    }
}
