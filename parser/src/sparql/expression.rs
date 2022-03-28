use crate::prelude::*;
use crate::sparql::common::{tag_no_case_no_space, tag_no_space, var};
use crate::sparql::path::{path as common_path, Path};
use crate::triple_common_parser::literal::literal as common_literal;
use crate::triple_common_parser::Literal;
use nom::sequence::delimited;
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}
#[derive(Debug, PartialEq, Clone)]
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
fn pair_expr<'a, F, F2, F3>(
    sep: F,
    left_expr: F2,
    right_expr: F3,
) -> impl FnMut(&'a str) -> ParserResult<(Expr<'a>, Expr<'a>)>
where
    F: FnMut(&'a str) -> ParserResult<&'a str>,
    F2: FnMut(&'a str) -> ParserResult<Expr>,
    F3: FnMut(&'a str) -> ParserResult<Expr>,
{
    separated_pair(left_expr, sep, right_expr)
}

fn bracketed(s: &str) -> ParserResult<Expr> {
    preceded(
        char('('),
        map(terminated(expr, char(')')), |ex| {
            Expr::Bracketed(Box::new(ex))
        }),
    )(s)
}
pub(super) fn list(s: &str) -> ParserResult<Expr> {
    preceded(
        char('('),
        terminated(
            map(
                separated_list1(char(','), alt((variable, literal, path))),
                |list| Expr::List(VecDeque::from(list)),
            ),
            char(')'),
        ),
    )(s)
}
fn relational<'a>(s: &'a str) -> ParserResult<Expr<'a>> {
    fn make_rel<'a>(op: RelationalOperator) -> impl Fn((Expr<'a>, Expr<'a>)) -> Expr<'a> {
        move |(left, right)| Expr::Relational {
            left: Box::new(left),
            operator: op.clone(),
            right: Box::new(right),
        }
    }
    alt((
        map(
            pair_expr(tag_no_space("<="), additive, additive),
            make_rel(RelationalOperator::LowerOrEqual),
        ),
        map(
            pair_expr(tag_no_space(">="), additive, additive),
            make_rel(RelationalOperator::GreaterOrEqual),
        ),
        map(
            pair_expr(tag_no_space("<"), additive, additive),
            make_rel(RelationalOperator::Lower),
        ),
        map(
            pair_expr(tag_no_space(">"), additive, additive),
            make_rel(RelationalOperator::Greater),
        ),
        map(
            pair_expr(tag_no_space("!="), additive, additive),
            make_rel(RelationalOperator::Diff),
        ),
        map(
            pair_expr(tag_no_space("="), additive, additive),
            make_rel(RelationalOperator::Equals),
        ),
        map(
            pair_expr(tag_no_case_no_space("NOT IN"), additive, list),
            make_rel(RelationalOperator::NotIn),
        ),
        map(
            pair_expr(tag_no_case_no_space("IN"), additive, list),
            make_rel(RelationalOperator::In),
        ),
    ))(s)
}
pub(super) fn additive(s: &str) -> ParserResult<Expr> {
    alt((bracketed, literal, variable))(s)
}
fn arithmetic<'a>(s: &'a str) -> ParserResult<Expr<'a>> {
    fn make_rel<'a>(op: ArithmeticOperator) -> impl Fn((Expr<'a>, Expr<'a>)) -> Expr<'a> {
        move |(left, right)| Expr::Arithmetic {
            left: Box::new(left),
            operator: op.clone(),
            right: Box::new(right),
        }
    }

    alt((
        map(
            pair_expr(tag_no_space("+"), additive, additive),
            make_rel(ArithmeticOperator::Add),
        ),
        map(
            pair_expr(tag_no_space("-"), additive, additive),
            make_rel(ArithmeticOperator::Subtract),
        ),
        map(
            pair_expr(tag_no_space("/"), additive, additive),
            make_rel(ArithmeticOperator::Divide),
        ),
        map(
            pair_expr(tag_no_space("*"), additive, additive),
            make_rel(ArithmeticOperator::Multiply),
        ),
    ))(s)
}
fn conditional(s: &str) -> ParserResult<Expr> {
    let tag_no_space = |s| delimited(multispace0, tag(s), multispace0);

    alt((
        map(
            pair_expr(tag_no_space("||"), relational, relational),
            |(left, right)| Expr::ConditionalOr {
                left: Box::new(left),
                right: Box::new(right),
            },
        ),
        map(
            pair_expr(tag_no_space("&&"), relational, relational),
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
        variable,
        path,
        literal,
    ))(s)
}

#[cfg(test)]
mod test {
    use crate::sparql::expression::expr;
    use crate::sparql::expression::ArithmeticOperator::Multiply;
    use crate::sparql::expression::Expr;
    use crate::sparql::expression::Expr::{Arithmetic, Bracketed, Relational};
    use crate::sparql::expression::RelationalOperator;
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
}
