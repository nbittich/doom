use crate::prelude::*;
pub(crate) fn var(s: &str) -> ParserResult<&str> {
    terminated(
        preceded(multispace0, preceded(tag("?").or(tag("$")), alphanumeric1)),
        multispace0,
    )(s)
}
pub(super) fn tag_no_space<'a>(s: &'a str) -> impl FnMut(&'a str) -> ParserResult<&'a str> {
    delimited(multispace0, tag(s), multispace0)
}
pub(super) fn tag_no_case_no_space<'a>(s: &'a str) -> impl FnMut(&'a str) -> ParserResult<&'a str> {
    delimited(multispace0, tag_no_case(s), multispace0)
}
