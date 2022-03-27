use crate::prelude::*;
pub(crate) fn var(s: &str) -> ParserResult<&str> {
    terminated(
        preceded(multispace0, preceded(tag("?").or(tag("$")), alphanumeric1)),
        multispace0,
    )(s)
}
