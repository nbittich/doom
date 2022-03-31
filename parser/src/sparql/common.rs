use crate::prelude::*;
use crate::triple_common_parser::{tag_no_case_no_space, tag_no_space};

pub(crate) fn var(s: &str) -> ParserResult<&str> {
    terminated(
        preceded(multispace0, preceded(tag("?").or(tag("$")), alphanumeric1)),
        multispace0,
    )(s)
}

pub(super) fn parameterized_func<'a, T, E, F, F2>(
    func_name: &'a str,
    expr_parser: F2,
    mapper: F,
) -> impl FnMut(&'a str) -> ParserResult<E>
where
    F: FnMut(T) -> E,
    F2: FnMut(&'a str) -> ParserResult<T>,
{
    map(
        preceded(
            tag_no_case_no_space(func_name),
            preceded(
                tag_no_space("("),
                terminated(expr_parser, tag_no_space(")")),
            ),
        ),
        mapper,
    )
}
pub(super) fn single_parameter_func<'a, T, E, F, F2>(
    func_name: &'a str,
    expr_parser: F2,
    mapper: F,
) -> impl FnMut(&'a str) -> ParserResult<E>
where
    F: FnMut(T) -> E,
    F2: FnMut(&'a str) -> ParserResult<T>,
{
    parameterized_func(func_name, expr_parser, mapper)
}
pub(super) fn two_parameter_func<'a, T, F, F2, F3>(
    func_name: &'a str,
    left_param_parser: F2,
    right_param_parser: F3,
    mapper: F,
) -> impl FnMut(&'a str) -> ParserResult<T>
where
    F: FnMut((T, T)) -> T,
    F2: FnMut(&'a str) -> ParserResult<T>,
    F3: FnMut(&'a str) -> ParserResult<T>,
{
    parameterized_func(
        func_name,
        separated_pair(left_param_parser, tag_no_space(","), right_param_parser),
        mapper,
    )
}
