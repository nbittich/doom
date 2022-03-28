use crate::prelude::*;
use crate::sparql::common::{tag_no_case_no_space, tag_no_space};
use crate::sparql::expression::{additive, expr, list, literal, path, variable, BuiltInCall, Expr};

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
fn abs(s: &str) -> ParserResult<Expr> {
    single_parameter_func("ABS", additive, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Abs(exp)))
    })(s)
}
fn ceil(s: &str) -> ParserResult<Expr> {
    single_parameter_func("CEIL", additive, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Ceil(exp)))
    })(s)
}
fn floor(s: &str) -> ParserResult<Expr> {
    single_parameter_func("FLOOR", additive, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Floor(exp)))
    })(s)
}
fn round(s: &str) -> ParserResult<Expr> {
    single_parameter_func("ROUND", additive, |exp| {
        Expr::BuiltInCall(Box::new(BuiltInCall::Floor(exp)))
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
