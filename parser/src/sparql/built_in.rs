use crate::prelude::*;
use crate::sparql::common::{parameterized_func, single_parameter_func, two_parameter_func};
use crate::sparql::expression::{expr, list, literal, path, variable, BuiltInCall, Expr};
use crate::triple_common_parser::{tag_no_case_no_space, tag_no_space};

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
fn abs(s: &str) -> ParserResult<Expr> {
    single_parameter_func("ABS", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Abs(exp)))
    })(s)
}
fn ceil(s: &str) -> ParserResult<Expr> {
    single_parameter_func("CEIL", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Ceil(exp)))
    })(s)
}
fn floor(s: &str) -> ParserResult<Expr> {
    single_parameter_func("FLOOR", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Floor(exp)))
    })(s)
}
fn round(s: &str) -> ParserResult<Expr> {
    single_parameter_func("ROUND", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Round(exp)))
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

fn str_lang(s: &str) -> ParserResult<Expr> {
    two_parameter_func("STRLANG", expr, expr, |(left, right)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrLang { left, right }))
    })(s)
}

fn str_dt(s: &str) -> ParserResult<Expr> {
    two_parameter_func("STRDT", expr, expr, |(left, right)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrDt { left, right }))
    })(s)
}
fn same_term(s: &str) -> ParserResult<Expr> {
    two_parameter_func("sameTerm", expr, expr, |(left, right)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::SameTerm { left, right }))
    })(s)
}

fn concat(s: &str) -> ParserResult<Expr> {
    map(preceded(tag_no_case_no_space("CONCAT"), list), |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Concat(exp)))
    })(s)
}

fn str_len(s: &str) -> ParserResult<Expr> {
    single_parameter_func("STRLEN", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrLen(exp)))
    })(s)
}

fn is_iri(s: &str) -> ParserResult<Expr> {
    single_parameter_func("isIRI", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::IsIri(exp)))
    })(s)
}
fn is_uri(s: &str) -> ParserResult<Expr> {
    single_parameter_func("isURI", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::IsIri(exp)))
    })(s)
}
fn is_blank(s: &str) -> ParserResult<Expr> {
    single_parameter_func("isBLANK", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::IsBlank(exp)))
    })(s)
}
fn is_literal(s: &str) -> ParserResult<Expr> {
    single_parameter_func("isLITERAL", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::IsLiteral(exp)))
    })(s)
}
fn is_numeric(s: &str) -> ParserResult<Expr> {
    single_parameter_func("isNUMERIC", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::IsNumeric(exp)))
    })(s)
}
fn ucase(s: &str) -> ParserResult<Expr> {
    single_parameter_func("UCASE", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::UCase(exp)))
    })(s)
}
fn lcase(s: &str) -> ParserResult<Expr> {
    single_parameter_func("LCASE", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::LCase(exp)))
    })(s)
}
fn encode_for_uri(s: &str) -> ParserResult<Expr> {
    single_parameter_func("ENCODE_FOR_URI", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::EncodeForUri(exp)))
    })(s)
}
fn str_starts(s: &str) -> ParserResult<Expr> {
    two_parameter_func("STRSTARTS", expr, expr, |(left, right)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrStarts { left, right }))
    })(s)
}
fn str_ends(s: &str) -> ParserResult<Expr> {
    two_parameter_func("STRENDS", expr, expr, |(left, right)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrEnds { left, right }))
    })(s)
}
fn str_before(s: &str) -> ParserResult<Expr> {
    two_parameter_func("STRBEFORE", expr, expr, |(left, right)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrBefore { left, right }))
    })(s)
}
fn str_after(s: &str) -> ParserResult<Expr> {
    two_parameter_func("STRAFTER", expr, expr, |(left, right)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrAfter { left, right }))
    })(s)
}

fn year(s: &str) -> ParserResult<Expr> {
    single_parameter_func("YEAR", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Year(exp)))
    })(s)
}
fn month(s: &str) -> ParserResult<Expr> {
    single_parameter_func("MONTH", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Month(exp)))
    })(s)
}
fn day(s: &str) -> ParserResult<Expr> {
    single_parameter_func("DAY", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Day(exp)))
    })(s)
}
fn hours(s: &str) -> ParserResult<Expr> {
    single_parameter_func("HOURS", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Hours(exp)))
    })(s)
}
fn minutes(s: &str) -> ParserResult<Expr> {
    single_parameter_func("MINUTES", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Minutes(exp)))
    })(s)
}
fn seconds(s: &str) -> ParserResult<Expr> {
    single_parameter_func("SECONDS", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Seconds(exp)))
    })(s)
}
fn timezone(s: &str) -> ParserResult<Expr> {
    single_parameter_func("TIMEZONE", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Timezone(exp)))
    })(s)
}
fn tz(s: &str) -> ParserResult<Expr> {
    single_parameter_func("TZ", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Tz(exp)))
    })(s)
}
fn now(s: &str) -> ParserResult<Expr> {
    single_parameter_func("NOW", space0, |_| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Now))
    })(s)
}
fn uuid(s: &str) -> ParserResult<Expr> {
    single_parameter_func("UUID", space0, |_| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Uuid))
    })(s)
}
fn str_uuid(s: &str) -> ParserResult<Expr> {
    single_parameter_func("STRUUID", space0, |_| {
        Expr::BuiltInCall(Box::new(BuiltInCall::StrUuid))
    })(s)
}
fn md5(s: &str) -> ParserResult<Expr> {
    single_parameter_func("MD5", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::MD5(exp)))
    })(s)
}
fn sha1(s: &str) -> ParserResult<Expr> {
    single_parameter_func("SHA1", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Sha1(exp)))
    })(s)
}
fn sha256(s: &str) -> ParserResult<Expr> {
    single_parameter_func("SHA256", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Sha256(exp)))
    })(s)
}
fn sha384(s: &str) -> ParserResult<Expr> {
    single_parameter_func("SHA384", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Sha384(exp)))
    })(s)
}
fn sha512(s: &str) -> ParserResult<Expr> {
    single_parameter_func("SHA512", expr, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Sha512(exp)))
    })(s)
}
fn coalesce(s: &str) -> ParserResult<Expr> {
    map(preceded(tag_no_case_no_space("COALESCE"), list), |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Coalesce(exp)))
    })(s)
}
fn if_func(s: &str) -> ParserResult<Expr> {
    let tuple_expr = |s| {
        tuple((
            terminated(expr, tag_no_space(",")),
            terminated(expr, tag_no_space(",")),
            expr,
        ))(s)
    };
    parameterized_func("IF", tuple_expr, |(first, second, third)| {
        Expr::BuiltInCall(Box::new(BuiltInCall::If {
            first,
            second,
            third,
        }))
    })(s)
}

pub(crate) fn built_in_call(s: &str) -> ParserResult<Expr> {
    alt((
        str,
        lang,
        data_type,
        bound,
        iri,
        uri,
        abs,
        ceil,
        floor,
        round,
        b_node,
        rand,
        lang_matches,
        str_lang,
        str_dt,
        same_term,
        concat,
        str_len,
        alt((
            is_iri,
            is_uri,
            is_blank,
            is_literal,
            is_numeric,
            ucase,
            lcase,
            encode_for_uri,
            str_starts,
            str_ends,
            str_before,
            str_after,
            alt((
                year, month, day, hours, minutes, seconds, timezone, tz, now, uuid, str_uuid, md5,
                sha1, sha256, sha384, sha512, coalesce, if_func,
            )),
        )),
    ))(s)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::sparql::expression::ArithmeticOperator;
    use crate::sparql::expression::ArithmeticOperator::{Divide, Subtract};
    use crate::sparql::expression::BuiltInCall::{
        Abs, Coalesce, Concat, DataType, Day, EncodeForUri, Floor, Hours, If, IsBlank, IsIri,
        IsLiteral, IsNumeric, LCase, Lang, LangMatches, Minutes, Month, Now, SameTerm, Seconds,
        Sha1, Sha256, Sha384, Sha512, Str, StrAfter, StrBefore, StrDt, StrEnds, StrLang, StrLen,
        StrStarts, StrUuid, Timezone, Tz, UCase, Uuid, Year, MD5,
    };
    use crate::sparql::expression::Expr::{
        Arithmetic, Bracketed, List, Literal as ExprLit, Path, Variable,
    };
    use crate::sparql::path::Path::Iri as PathIri;
    use crate::triple_common_parser::Literal::Decimal;
    use std::collections::VecDeque;

    use crate::triple_common_parser::Iri::{Enclosed, Prefixed};
    macro_rules! a_box {
        ($a:expr) => {
            Box::new($a)
        };
    }
    #[test]
    fn test_str() {
        let s = "STR (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Str(Variable("s",),))), res);
        let s = "STR( <http://xx.com> )";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(Str(Path(PathIri(Enclosed("http://xx.com"))),))),
            res
        );
    }

    #[test]
    fn test_lang() {
        let s = "Lang (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Lang(Variable("s",),))), res);

        let s = "lang( <http://xx.com> )";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(Lang(Path(PathIri(Enclosed("http://xx.com"))),))),
            res
        );
    }
    #[test]
    fn test_data_type() {
        let s = "DATATYPE (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(DataType(Variable("s",),))), res);

        let s = "DATATYPE( <http://xx.com> )";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(DataType(Path(PathIri(Enclosed("http://xx.com"))),))),
            res
        );
    }
    #[test]
    fn test_bound() {
        let s = "bound (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(BuiltInCall::Bound(Variable("s",),))),
            res
        );
    }
    #[test]
    fn test_iri() {
        let s = "URI (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(BuiltInCall::Iri(Variable("s",),))),
            res
        );

        let s = "IRI (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(BuiltInCall::Iri(Variable("s",),))),
            res
        );
    }
    #[test]
    fn test_abs() {
        let s = "ABS ( ?s + ?p )";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(Abs(Arithmetic {
                left: a_box!(Variable("s",)),
                operator: ArithmeticOperator::Add,
                right: a_box!(Variable("p",)),
            },)),),
            res
        )
    }
    #[test]
    fn test_floor() {
        let s = "floor(?s * 1.0)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(Floor(Arithmetic {
                left: a_box!(Variable("s",)),
                operator: ArithmeticOperator::Multiply,
                right: a_box!(ExprLit(Decimal(1.0))),
            },)),),
            res
        )
    }
    #[test]
    fn test_ceil() {
        let s = "ceil( ?s / ?p )";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(BuiltInCall::Ceil(Arithmetic {
                left: a_box!(Variable("s",)),
                operator: ArithmeticOperator::Divide,
                right: a_box!(Variable("p",)),
            },)),),
            res
        )
    }
    #[test]
    fn test_round() {
        let s = "ROUND   ( ?s/?p)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(BuiltInCall::Round(Arithmetic {
                left: a_box!(Variable("s",)),
                operator: ArithmeticOperator::Divide,
                right: a_box!(Variable("p",)),
            },)),),
            res
        )
    }
    #[test]
    fn test_b_node() {
        let s = "bnode   ( )";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(BuiltInCall::BNode(None)),), res);
        let s = "bnode( foaf:name)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(BuiltInCall::BNode(Some(Path(PathIri(Prefixed {
                prefix: "foaf",
                local_name: "name"
            }))))),),
            res
        )
    }
    #[test]
    fn test_rand() {
        let s = "RAND()";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(BuiltInCall::Rand),), res);
    }
    #[test]
    fn test_lang_matches() {
        // todo
        // just for testing, obviously this doesn't make sense
        // and should be catch when executing query...
        // ...or when I decide to read the spec entirely
        let s = "LANGMATCHES(?s, (?s - ?x))";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(LangMatches {
                left: Variable("s",),
                right: Bracketed(a_box!(Arithmetic {
                    left: a_box!(Variable("s",)),
                    operator: Subtract,
                    right: a_box!(Variable("x",)),
                }),),
            })),
            res
        )
    }
    #[test]
    fn test_str_lang() {
        // todo
        // just for testing, obviously this doesn't make sense
        // and should be catch when executing query...
        // ...or when I decide to read the spec entirely
        let s = "STRLANG(?s, (?s - ?x))";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(StrLang {
                left: Variable("s",),
                right: Bracketed(a_box!(Arithmetic {
                    left: a_box!(Variable("s",)),
                    operator: Subtract,
                    right: a_box!(Variable("x",)),
                }),),
            })),
            res
        )
    }
    #[test]
    fn test_str_dt() {
        // todo
        // just for testing, obviously this doesn't make sense
        // and should be catch when executing query...
        // ...or when I decide to read the spec entirely
        let s = "STRDT(?s, (?s / ?x))";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(StrDt {
                left: Variable("s",),
                right: Bracketed(a_box!(Arithmetic {
                    left: a_box!(Variable("s",)),
                    operator: Divide,
                    right: a_box!(Variable("x",)),
                }),),
            })),
            res
        )
    }
    #[test]
    fn test_same_term() {
        // todo
        // just for testing, obviously this doesn't make sense
        // and should be catch when executing query...
        // ...or when I decide to read the spec entirely
        let s = "sameterm(?s, (?s / ?x))";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(SameTerm {
                left: Variable("s",),
                right: Bracketed(a_box!(Arithmetic {
                    left: a_box!(Variable("s",)),
                    operator: Divide,
                    right: a_box!(Variable("x",)),
                }),),
            })),
            res
        )
    }
    #[test]
    fn test_concat() {
        let s = "CONCAT (?s,?p,?o)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(Concat(List(VecDeque::from(vec![
                Variable("s",),
                Variable("p",),
                Variable("o",),
            ]),),)),),
            res
        )
    }
    #[test]
    fn test_strlen() {
        let s = "STRLEN (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(StrLen(Variable("s",)))), res)
    }
    #[test]
    fn test_is_iri() {
        let s = "ISIRI (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(IsIri(Variable("s",)))), res);
        let s = "isUri (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(IsIri(Variable("s",)))), res);
    }
    #[test]
    fn test_is_blank() {
        let s = "isBlank (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(IsBlank(Variable("s",)))), res);
    }
    #[test]
    fn test_is_literal() {
        let s = "isLITERAL (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(IsLiteral(Variable("s",)))), res);
    }
    #[test]
    fn test_is_numeric() {
        let s = "isNUMERIC (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(IsNumeric(Variable("s",)))), res);
    }
    #[test]
    fn test_ucase() {
        let s = "UCASE (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(UCase(Variable("s",)))), res);
    }
    #[test]
    fn test_lcase() {
        let s = "LCASE (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(LCase(Variable("s",)))), res);
    }
    #[test]
    fn test_encode_for_uri() {
        let s = "ENCODE_FOR_URI(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(EncodeForUri(Variable("s",)))), res);
    }
    #[test]
    fn test_str_starts() {
        let s = "STRSTARTS(?s, ?p)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(StrStarts {
                left: Variable("s",),
                right: Variable("p",),
            })),
            res
        );
    }
    #[test]
    fn test_str_ends() {
        let s = "STRENDS(?s, ?p)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(StrEnds {
                left: Variable("s",),
                right: Variable("p",),
            })),
            res
        );
    }
    #[test]
    fn test_str_before() {
        let s = "STRBEFORE(?s, ?p)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(StrBefore {
                left: Variable("s",),
                right: Variable("p",),
            })),
            res
        );
    }
    #[test]
    fn test_str_after() {
        let s = "STRAFTER(?s, ?p)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(StrAfter {
                left: Variable("s",),
                right: Variable("p",),
            })),
            res
        );
    }

    #[test]
    fn test_year() {
        let s = "YEAR (?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Year(Variable("s",)))), res);
    }
    #[test]
    fn test_month() {
        let s = "month(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Month(Variable("s",)))), res);
    }
    #[test]
    fn test_day() {
        let s = "DAY(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Day(Variable("s",)))), res);
    }
    #[test]
    fn test_hours() {
        let s = "HOURS(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Hours(Variable("s",)))), res);
    }
    #[test]
    fn test_minutes() {
        let s = "minutes(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Minutes(Variable("s",)))), res);
    }
    #[test]
    fn test_seconds() {
        let s = "seconds(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Seconds(Variable("s",)))), res);
    }
    #[test]
    fn test_timezone() {
        let s = "timezone(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Timezone(Variable("s",)))), res);
    }
    #[test]
    fn test_tz() {
        let s = "TZ(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Tz(Variable("s",)))), res);
    }
    #[test]
    fn test_now() {
        let s = "NOw()";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Now)), res);
    }
    #[test]
    fn test_uuid() {
        let s = "UUID()";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Uuid)), res);
    }
    #[test]
    fn test_str_uuid() {
        let s = "STRUUID()";
        let (_, res) = str_uuid(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(StrUuid)), res);
    }

    #[test]
    fn test_md5() {
        let s = "MD5(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(MD5(Variable("s",)))), res);
    }

    #[test]
    fn test_sha1() {
        let s = "SHA1(?s)";
        let (_, res) = sha1(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Sha1(Variable("s",)))), res);
    }

    #[test]
    fn test_sha256() {
        let s = "sha256(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Sha256(Variable("s",)))), res);
    }
    #[test]
    fn test_sha384() {
        let s = "sha384(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Sha384(Variable("s",)))), res);
    }
    #[test]
    fn test_sha512() {
        let s = "sha512(?s)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(Expr::BuiltInCall(a_box!(Sha512(Variable("s",)))), res);
    }

    #[test]
    fn test_coalesce() {
        let s = "COALESCE (?s,?p,?o)";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(Coalesce(List(VecDeque::from(vec![
                Variable("s",),
                Variable("p",),
                Variable("o",),
            ]),),)),),
            res
        )
    }
    #[test]
    fn test_if() {
        let s = "IF (?s, ?p, ?o )";
        let (_, res) = built_in_call(s).unwrap();
        assert_eq!(
            Expr::BuiltInCall(a_box!(If {
                first: Variable("s",),
                second: Variable("p",),
                third: Variable("o",),
            })),
            res
        )
    }
}
