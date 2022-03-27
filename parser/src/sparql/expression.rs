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
    Count,
    Sum,
    Min,
    Max,
    Avg,
    Sample,
    GroupConcat,
    Str(Expr<'a>),
    Lang(Expr<'a>),
    LangMatches { left: Expr<'a>, right: Expr<'a> },
    DataType(Expr<'a>),
    Bound(Expr<'a>),
    Iri(Expr<'a>),
    BNode(Option<Expr<'a>>),
    Rand,
    Abs,
    Ceil,
    Floor,
    Concat,
    SubStr,
    StrLen,
    Replace,
    UCase,
    LCase,
    EncodeForUri,
    Contains,
    StrStarts,
    StrEnds,
    StrBefore,
    StrAfter,
    Year,
    Month,
    Day,
    Hours,
    Minutes,
    Seconds,
    Timezone,
    TZ,
    Now,
    Uuid,
    StrUuid,
    MD5,
    Sha1,
    Sha256,
    Sha384,
    Sha512,
    Coalesce,
    If,
    StrLang,
    StrDt,
    SameTerm,
    IsIri, // == IsUri
    IsBlank,
    IsLiteral,
    IsNumeric,
    Regex,
    Exists,
    NotExists,
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
fn list(s: &str) -> ParserResult<Expr> {
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
fn additive(s: &str) -> ParserResult<Expr> {
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
fn literal(s: &str) -> ParserResult<Expr> {
    map(common_literal, Expr::Literal)(s)
}
fn path(s: &str) -> ParserResult<Expr> {
    map(common_path, Expr::Path)(s)
}
fn variable(s: &str) -> ParserResult<Expr> {
    map(var, Expr::Variable)(s)
}

mod built_in_call {
    use crate::prelude::*;
    use crate::sparql::common::{tag_no_case_no_space, tag_no_space};
    use crate::sparql::expression::{expr, literal, path, variable, BuiltInCall, Expr};

    fn parameterized_func<'a, T, E, F, F2>(
        func_name: &'a str,
        expr_parser: F2,
        mapper: F,
    ) -> impl FnMut(&'a str) -> ParserResult<E>
    where
        F: FnMut(T) -> E + Copy,
        F2: FnMut(&'a str) -> ParserResult<T> + Copy,
    {
        move |s| {
            map(
                preceded(
                    tag_no_case_no_space(func_name),
                    delimited(tag_no_space("("), expr_parser, tag_no_space(")")),
                ),
                mapper,
            )(s)
        }
    }
    fn single_parameter_func<'a, T, E, F, F2>(
        func_name: &'a str,
        expr_parser: F2,
        mapper: F,
    ) -> impl FnMut(&'a str) -> ParserResult<E>
    where
        F: FnMut(T) -> E + Copy,
        F2: FnMut(&'a str) -> ParserResult<T> + Copy,
    {
        move |s| parameterized_func(func_name, expr_parser, mapper)(s)
    }
    fn two_parameter_func<'a, F, F2, F3>(
        func_name: &'a str,
        left_param_parser: F2,
        right_param_parser: F3,
        mapper: F,
    ) -> impl FnMut(&'a str) -> ParserResult<Expr<'a>>
    where
        F: FnMut((Expr<'a>, Expr<'a>)) -> Expr<'a> + Copy,
        F2: FnMut(&'a str) -> ParserResult<Expr<'a>> + Copy,
        F3: FnMut(&'a str) -> ParserResult<Expr<'a>> + Copy,
    {
        move |s: &'a str| {
            let separate_expr =
                |s| separated_pair(left_param_parser, tag_no_space(","), right_param_parser)(s);
            parameterized_func(func_name, separate_expr, mapper)(s)
        }
    }
    fn str(s: &str) -> ParserResult<Expr> {
        single_parameter_func("STR", expr, |exp| {
            Expr::BuiltInCall(Box::new(BuiltInCall::Str(exp)))
        })(s)
    }
    fn lang(s: &str) -> ParserResult<Expr> {
        single_parameter_func("LANG", expr, |exp| {
            Expr::BuiltInCall(Box::new(BuiltInCall::Lang(exp)))
        })(s)
    }
    fn data_type(s: &str) -> ParserResult<Expr> {
        single_parameter_func("DATATYPE", expr, |exp| {
            Expr::BuiltInCall(Box::new(BuiltInCall::DataType(exp)))
        })(s)
    }
    fn bound(s: &str) -> ParserResult<Expr> {
        single_parameter_func("BOUND", variable, |exp| {
            Expr::BuiltInCall(Box::new(BuiltInCall::Bound(exp)))
        })(s)
    }
    fn iri(s: &str) -> ParserResult<Expr> {
        single_parameter_func("IRI", expr, |exp| {
            Expr::BuiltInCall(Box::new(BuiltInCall::Iri(exp)))
        })(s)
    }
    fn uri(s: &str) -> ParserResult<Expr> {
        single_parameter_func("URI", expr, |exp| {
            Expr::BuiltInCall(Box::new(BuiltInCall::Iri(exp)))
        })(s)
    }
    fn b_node(s: &str) -> ParserResult<Expr> {
        let op = |s| opt(alt((literal, path)))(s);
        single_parameter_func("BNODE", op, |exp| {
            Expr::BuiltInCall(Box::new(BuiltInCall::BNode(exp)))
        })(s)
    }
    fn rand(s: &str) -> ParserResult<Expr> {
        single_parameter_func("RAND", space0, |_| {
            Expr::BuiltInCall(Box::new(BuiltInCall::Rand))
        })(s)
    }
    fn lang_matches(s: &str) -> ParserResult<Expr> {
        two_parameter_func("LANGMATCHES", expr, expr, |(left, right)| {
            Expr::BuiltInCall(Box::new(BuiltInCall::LangMatches { left, right }))
        })(s)
    }
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
