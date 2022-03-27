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

    pub(crate) fn parse_boolean(s: &str) -> ParserResult<bool> {
        let (remaining, val) = terminated(
            map(alt((tag("true"), tag("false"))), |b: &str| {
                b.parse::<bool>().map_err(|_err| {
                    let err: Error<&str> = make_error(b, ErrorKind::IsNot);
                    nom::Err::Error(err)
                })
            }),
            multispace0,
        )(s)?;
        let boolean_value = val?;
        Ok((remaining, boolean_value))
    }

    pub(crate) fn parse_number(s: &str) -> ParserResult<Literal> {
        fn try_parse_int(s: &str) -> ParserResult<i64> {
            all_consuming(I64)(s)
        }
        fn try_parse_decimal(s: &str) -> ParserResult<f32> {
            all_consuming(float)(s)
        }
        fn try_parse_double(s: &str) -> ParserResult<f64> {
            all_consuming(double)(s)
        }
        let (remaining, num) = recognize_float(s)?;
        if let Ok((_, n)) = try_parse_int(num) {
            Ok((remaining, Literal::Integer(n)))
        } else if let Ok((_, n)) = try_parse_decimal(num) {
            Ok((remaining, Literal::Decimal(n)))
        } else if let Ok((_, n)) = try_parse_double(num) {
            Ok((remaining, Literal::Double(n)))
        } else {
            let err: Error<&str> = make_error(s, ErrorKind::IsNot);
            Err(nom::Err::Error(err))
        }
    }

    pub(crate) fn primitive_literal(s: &str) -> ParserResult<Literal> {
        let (no_white_space, _) = multispace0(s)?;

        return if let Ok((remaining, boolean_value)) = parse_boolean(no_white_space) {
            Ok((remaining, Literal::Boolean(boolean_value)))
        } else if let Ok((remaining, number_value)) = parse_number(no_white_space) {
            Ok((remaining, number_value))
        } else {
            let err: Error<&str> = make_error(no_white_space, ErrorKind::IsNot);
            Err(nom::Err::Error(err))
        };
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

    pub(crate) fn literal(s: &str) -> ParserResult<Literal> {
        alt((string_literal, primitive_literal))(s)
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
        F1: FnMut(&'a str) -> ParserResult<'a, T> + Copy,
        F2: FnMut(Vec<T>) -> T,
    {
        move |s| {
            let (remaining, mut list) = separated_list1(char(','), object_extractor)(s)?;
            if list.len() > 1 {
                Ok((remaining, map_list(list)))
            } else if let Some(single_value) = list.pop() {
                Ok((remaining, single_value))
            } else {
                let err: Error<&str> = make_error(s, ErrorKind::LengthValue);
                Err(nom::Err::Error(err))
            }
        }
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
        F2: Fn(&'a str) -> ParserResult<'a, T> + Copy,
        F3: Fn(&'a str) -> ParserResult<'a, T> + Copy,
        F4: Fn((T, T)) -> T + Copy,
        F5: Fn(T, Vec<T>) -> T,
    {
        move |s| {
            let (remaining, subject) = subject_extractor(s)?;

            let (remaining, list) = preceded(
                multispace0,
                separated_list1(
                    delimited(multispace0, tag(";"), comments),
                    map(
                        pair(predicate_extractor, object_list_extractor),
                        map_predicate_object,
                    ),
                ),
            )(remaining)?;
            Ok((remaining, map_statement(subject, list)))
        }
    }
    pub(crate) fn collection<'a, T, F>(
        object_extractor: F,
    ) -> impl Fn(&'a str) -> ParserResult<'a, VecDeque<T>>
    where
        F: Fn(&'a str) -> ParserResult<'a, T> + Copy,
    {
        move |s| {
            let (remaining, _) = multispace0(s)?;
            let (remaining, res) = preceded(
                char('('),
                terminated(
                    separated_list0(multispace1, object_extractor),
                    preceded(multispace0, cut(char(')'))),
                ),
            )(remaining)?;
            Ok((remaining, VecDeque::from(res)))
        }
    }
    pub(crate) fn anon_bnode<'a, F, T>(anon_parser: F) -> impl Fn(&'a str) -> ParserResult<'a, T>
    where
        F: Fn(&'a str) -> ParserResult<'a, T> + Copy,
    {
        move |s| {
            let extract = preceded(
                char('['),
                terminated(anon_parser, preceded(multispace0, cut(char(']')))),
            );
            preceded(multispace0, extract)(s)
        }
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
