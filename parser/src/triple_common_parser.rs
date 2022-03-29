use crate::prelude::{
    char, delimited, line_ending, many0, multispace0, preceded, take_until, ParserResult,
};
pub const BASE_TURTLE: &str = "@base";
pub const BASE_SPARQL: &str = "BASE";
pub const PREFIX_TURTLE: &str = "@prefix";
pub const PREFIX_SPARQL: &str = "PREFIX";

#[derive(PartialEq, Debug)]
pub enum Iri<'a> {
    Enclosed(&'a str),
    Prefixed {
        prefix: &'a str,
        local_name: &'a str,
    },
}
#[derive(PartialEq, Debug)]
pub enum Literal<'a> {
    Quoted {
        datatype: Option<Iri<'a>>,
        value: &'a str,
        lang: Option<&'a str>,
    },
    Double(f64),
    Decimal(f32),
    Integer(i64),
    Boolean(bool),
}
#[derive(PartialEq, Debug)]
pub enum BlankNode<'a> {
    Labeled(&'a str),
    Unlabeled,
}
pub(crate) mod iri {
    use crate::grammar::PN_LOCAL_ESC;
    use crate::prelude::*;
    use crate::triple_common_parser::Iri;
    use nom::bytes::complete::escaped;
    use nom::character::complete::one_of;
    pub(crate) fn prefixed_iri(s: &str) -> ParserResult<Iri> {
        let prefixed = map(
            separated_pair(
                take_while(|s: char| s.is_alphanumeric()),
                tag(":"),
                escaped(alphanumeric1, '\\', one_of(PN_LOCAL_ESC)),
            ),
            |(prefix, local_name)| Iri::Prefixed { prefix, local_name },
        );
        preceded(multispace0, prefixed)(s)
    }
    pub(crate) fn iri(s: &str) -> ParserResult<Iri> {
        delimited(multispace0, alt((prefixed_iri, enclosed_iri)), multispace0)(s)
    }
    pub(crate) fn enclosed_iri(s: &str) -> ParserResult<Iri> {
        let enclosed = map(
            delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
            Iri::Enclosed,
        );

        preceded(multispace0, enclosed)(s)
    }
}

pub(crate) mod prologue {
    use super::iri::enclosed_iri;
    use crate::prelude::*;
    use crate::triple_common_parser::{
        Iri, BASE_SPARQL, BASE_TURTLE, PREFIX_SPARQL, PREFIX_TURTLE,
    };

    pub(crate) fn base_sparql(s: &str) -> ParserResult<Iri> {
        base(BASE_SPARQL, enclosed_iri)(s)
    }
    pub(crate) fn base_turtle(s: &str) -> ParserResult<Iri> {
        base(
            BASE_TURTLE,
            terminated(enclosed_iri, preceded(multispace0, char('.'))),
        )(s)
    }
    fn base<'a, F>(
        base_tag: &'static str,
        extract_base: F,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, Iri<'a>>
    where
        F: FnMut(&'a str) -> ParserResult<'a, Iri<'a>>,
    {
        preceded(preceded(multispace0, tag_no_case(base_tag)), extract_base)
    }
    fn prefix<'a>(
        prefix_tag: &'static str,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, (&'a str, Iri<'a>)> {
        preceded(
            preceded(multispace0, tag_no_case(prefix_tag)),
            separated_pair(
                preceded(multispace0, take_until(":")),
                tag(":"),
                enclosed_iri,
            ),
        )
    }
    pub(crate) fn prefix_turtle(s: &str) -> ParserResult<(&str, Iri<'_>)> {
        terminated(prefix(PREFIX_TURTLE), preceded(multispace0, char('.')))(s)
    }
    pub(crate) fn prefix_sparql(s: &str) -> ParserResult<(&str, Iri<'_>)> {
        prefix(PREFIX_SPARQL)(s)
    }
}
pub(crate) mod literal {
    use crate::grammar::{
        LANGTAG, STRING_LITERAL_LONG_QUOTE, STRING_LITERAL_LONG_SINGLE_QUOTE, STRING_LITERAL_QUOTE,
        STRING_LITERAL_SINGLE_QUOTE,
    };
    use crate::prelude::*;
    use crate::shared::XSD_STRING;
    use crate::triple_common_parser::iri::iri;
    use crate::triple_common_parser::{Iri, Literal};
    pub(crate) fn parse_boolean<'a>(
        case_sensitive: bool,
    ) -> impl FnMut(&'a str) -> ParserResult<Literal<'a>> {
        move |s| {
            let extractor = |s| {
                if case_sensitive {
                    alt((tag("true"), tag("false")))(s)
                } else {
                    alt((tag_no_case("true"), tag_no_case("false")))(s)
                }
            };
            map(
                terminated(
                    map(extractor, |bv: &'a str| bv.eq_ignore_ascii_case("true")),
                    multispace0,
                ),
                Literal::Boolean,
            )(s)
        }
    }

    pub(crate) fn parse_number(s: &str) -> ParserResult<Literal> {
        map_parser(
            recognize_float,
            alt((
                map(all_consuming(I64), Literal::Integer),
                map(all_consuming(float), Literal::Decimal),
                map(all_consuming(double), Literal::Double),
            )),
        )(s)
    }

    pub(crate) fn primitive_literal_sparql(s: &str) -> ParserResult<Literal> {
        preceded(multispace0, alt((parse_boolean(false), parse_number)))(s)
    }
    pub(crate) fn primitive_literal_turtle(s: &str) -> ParserResult<Literal> {
        preceded(multispace0, alt((parse_boolean(true), parse_number)))(s)
    }

    pub(crate) fn string_literal(s: &str) -> ParserResult<Literal> {
        let single_quote_literal = delimited(
            tag(STRING_LITERAL_SINGLE_QUOTE),
            take_until1(STRING_LITERAL_SINGLE_QUOTE),
            tag(STRING_LITERAL_SINGLE_QUOTE),
        );
        let double_quote_literal = delimited(
            tag(STRING_LITERAL_QUOTE),
            take_until1(STRING_LITERAL_QUOTE),
            tag(STRING_LITERAL_QUOTE),
        );
        let long_single_quote_literal = delimited(
            tag(STRING_LITERAL_LONG_SINGLE_QUOTE),
            take_until1(STRING_LITERAL_LONG_SINGLE_QUOTE),
            tag(STRING_LITERAL_LONG_SINGLE_QUOTE),
        );
        let long_quote_literal = delimited(
            tag(STRING_LITERAL_LONG_QUOTE),
            take_until1(STRING_LITERAL_LONG_QUOTE),
            tag(STRING_LITERAL_LONG_QUOTE),
        );
        let mut datatype = preceded(tag("^^"), iri);

        fn lang(s: &str) -> ParserResult<&str> {
            preceded(tag(LANGTAG), take_while(|a: char| a.is_alpha() || a == '-'))(s)
        }

        let (no_white_space, _) = multispace0(s)?;
        let (remaining, string_literal) = alt((
            single_quote_literal,
            double_quote_literal,
            long_quote_literal,
            long_single_quote_literal,
        ))(no_white_space)?;

        if let Ok((remaining, datatype)) = datatype(remaining) {
            Ok((
                remaining,
                Literal::Quoted {
                    datatype: Some(datatype),
                    value: string_literal,
                    lang: None,
                },
            ))
        } else if let Ok((remaining, lang)) = lang(remaining) {
            Ok((
                remaining,
                Literal::Quoted {
                    datatype: None,
                    value: string_literal,
                    lang: Some(lang),
                },
            ))
        } else {
            Ok((
                remaining,
                Literal::Quoted {
                    datatype: Some(Iri::Enclosed(XSD_STRING)),
                    value: string_literal,
                    lang: None,
                },
            ))
        }
    }

    pub(crate) fn literal_turtle(s: &str) -> ParserResult<Literal> {
        alt((string_literal, primitive_literal_turtle))(s)
    }

    pub(crate) fn literal_sparql(s: &str) -> ParserResult<Literal> {
        alt((string_literal, primitive_literal_sparql))(s)
    }
}
pub(crate) mod triple {
    use crate::grammar::BLANK_NODE_LABEL;
    use crate::prelude::*;
    use crate::shared::NS_TYPE;
    use crate::triple_common_parser::{comments, BlankNode, Iri};
    use std::collections::VecDeque;

    pub(crate) fn object_list<'a, F1, F2, T>(
        object_extractor: F1,
        mut map_list: F2,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, T>
    where
        F1: FnMut(&'a str) -> ParserResult<'a, T>,
        F2: FnMut(Vec<T>) -> T,
    {
        map(
            separated_list1(char(','), object_extractor),
            move |mut list| {
                if list.len() > 1 {
                    map_list(list)
                } else {
                    list.pop().unwrap()
                }
            },
        )
    }
    pub(crate) fn ns_type(s: &str) -> ParserResult<Iri> {
        map(terminated(char('a'), multispace1), |_| {
            Iri::Enclosed(NS_TYPE)
        })(s)
    }
    pub(crate) fn predicate_list<'a, F1, F2, F3, F4, F5, T>(
        subject_extractor: F1,
        predicate_extractor: F2,
        object_list_extractor: F3,
        map_predicate_object: F4,
        map_statement: F5,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, T>
    where
        F1: Fn(&'a str) -> ParserResult<'a, T>,
        F2: Fn(&'a str) -> ParserResult<'a, T>,
        F3: Fn(&'a str) -> ParserResult<'a, T>,
        F4: Fn((T, T)) -> T,
        F5: Fn(T, Vec<T>) -> T,
    {
        map(
            pair(
                subject_extractor,
                preceded(
                    multispace0,
                    separated_list1(
                        delimited(multispace0, tag(";"), comments),
                        map(
                            pair(predicate_extractor, object_list_extractor),
                            map_predicate_object,
                        ),
                    ),
                ),
            ),
            move |(subject, list)| map_statement(subject, list),
        )
    }
    pub(crate) fn collection<'a, T, F>(
        object_extractor: F,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, VecDeque<T>>
    where
        F: Fn(&'a str) -> ParserResult<'a, T>,
    {
        preceded(
            multispace0,
            map(
                preceded(
                    char('('),
                    terminated(
                        separated_list0(multispace1, object_extractor),
                        preceded(multispace0, cut(char(')'))),
                    ),
                ),
                VecDeque::from,
            ),
        )
    }
    pub(crate) fn anon_bnode<'a, F, T>(anon_parser: F) -> impl FnMut(&'a str) -> ParserResult<'a, T>
    where
        F: Fn(&'a str) -> ParserResult<'a, T>,
    {
        preceded(
            multispace0,
            preceded(
                char('['),
                terminated(anon_parser, preceded(multispace0, cut(char(']')))),
            ),
        )
    }
    pub(crate) fn labeled_bnode(s: &str) -> ParserResult<BlankNode> {
        let mut parse_labeled_bnode = delimited(
            tag(BLANK_NODE_LABEL),
            take_while(|s: char| !s.is_whitespace()),
            space0,
        );
        let (remaining, _) = multispace0(s)?; // todo maybe remove this
        let (remaining, label) = parse_labeled_bnode(remaining)?;
        if label.starts_with('.') || label.ends_with('.') || label.starts_with('-') {
            let err: Error<&str> = make_error(label, ErrorKind::IsNot);
            return Err(nom::Err::Error(err));
        }
        Ok((remaining, BlankNode::Labeled(label)))
    }
}
pub(crate) fn comments(s: &str) -> ParserResult<Vec<&str>> {
    many0(delimited(
        multispace0,
        preceded(char('#'), take_until("\n")),
        line_ending,
    ))(s)
}
