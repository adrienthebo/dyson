use ast::*;
use nom::types::CompleteStr;
use nom::{AsChar, IResult, InputTakeAtPosition};

pub fn hsp<'a, T>(input: T) -> IResult<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    input.split_at_position(|item| !(item.as_char() == ' '))
}

macro_rules! hws (
    ($i:expr, $($args:tt)*) => (
        {
            use nom::Err;
            use nom::Convert;
            match sep!($i, hsp, $($args)*) {
                Err(e) => Err(e),
                Ok((i1, o)) => {
                    match (hsp)(i1) {
                        Err(e) => Err(Err::convert(e)),
                        Ok((i2,_))    => Ok((i2, o))
                    }
                }
            }
        }
    )
);

named!(pub multi_line_comment(CompleteStr) -> Comment,
    preceded!(
        tag!("/*"),
        flat_map!(
            take_until_and_consume!("*/"),
            parse_to!(Comment)
        )
    )
);

named!(pub single_line_comment(CompleteStr) -> Comment,
    do_parse!(
        _prefix: alt!(tag!("#") | tag!("//"))        >>
        s: take_till!(|ch| ch == '\r' || ch == '\n') >>
        _suffix: opt!(call!(nom::line_ending))       >>
        (Comment(s.to_string()))
    )
);

named!(pub comment(CompleteStr) -> Comment,
    alt!(multi_line_comment | single_line_comment)
);

pub fn ident_start<T>(input: T) -> IResult<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position(|item| item.as_char().is_xid_start())
}

pub fn ident_continue<T>(input: T) -> IResult<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    let out = input.split_at_position(|item| {
        let ch = item.as_char();
        !(ch.is_xid_continue() || ch == '-')
    });
    out
}

named!(identifier(CompleteStr) -> Identifier,
    flat_map!(
        hws!(
            recognize!(
                pair!(
                    verify!(
                        take!(1),
                        |s: CompleteStr| {
                            if let Some((_size, ref ch)) = s.char_indices().nth(0) {
                                ch.is_xid_start() || ch == &'_'
                            } else {
                                false
                            }
                        }
                    ),
                    ident_continue
                )
            )
        ),
        parse_to!(Identifier)
    )
);

named!(numericlit(CompleteStr) -> NumericLit,
    flat_map!(
        hws!(
            recognize!(
                tuple!(
                    nom::digit,
                    opt!(preceded!(char!('.'), nom::digit)),
                    opt!(preceded!(char!('e'), nom::digit))
                )
            )
        ),
        parse_to!(NumericLit)
    )
);

named!(pub attribute(CompleteStr) -> Attribute,
    map!(
        separated_pair!(
            identifier,
            char!('='),
            expression
        ),
        |(ident, expr)| { Attribute { ident, expr } }
    )
);

named!(pub blocklabels(CompleteStr) -> BlockLabels,
    many0!(identifier)
);

named!(pub block(CompleteStr) -> Block,
    do_parse!(
        ident: identifier                               >>
        labels: blocklabels                             >>
        _bodyopen: pair!(char!('{'), nom::line_ending)  >>
        inner: body                                     >>
        _bodyclose: pair!(char!('}'), nom::line_ending) >>
        (Block { ident, labels, body: inner })
    )
);

named!(pub bodyitem(CompleteStr) -> BodyItem,
    alt!(
        attribute => { |a| BodyItem::AttrItem(a) } |
        block     => { |b| BodyItem::BlockItem(b) }
    )
);

named!(pub body(CompleteStr) -> Body,
    map!(
        fold_many0!(bodyitem, BodyItems::new(), |mut acc: BodyItems, it: BodyItem| {
            acc.push(it);
            acc
        }),
        |v: BodyItems| { Body(v) }
    )
);

named!(pub literalvalue(CompleteStr) -> LiteralValue,
    hws!(
        alt!(
            numericlit    => { |nl| LiteralValue::NumericLit(nl) } |
            tag!("true")  => { |_| LiteralValue::True } |
            tag!("false") => { |_| LiteralValue::False } |
            tag!("null")  => { |_| LiteralValue::Null }
        )
    )
);

named!(pub tuple(CompleteStr) -> Tuple,
    hws!(
        do_parse!(
            char!('[')                                        >>
            list: many0!(terminated!(expression, char!(','))) >>
            last: opt!(expression)                            >>
            char!(']')                                        >>
            ({
                let mut list = list;
                if let Some(item) = last {
                    list.push(item);
                }
                list
            })
        )
    )
);

named!(pub objectelem(CompleteStr) -> ObjectElem,
    hws!(
        do_parse!(
            key: alt!(
                identifier => { |i| ObjectKey::Identifier(i) } |
                expression => { |e| ObjectKey::Expression(e) }
            )                 >>
            char!('=')        >>
            value: expression >>
            (ObjectElem { key, value })
        )
    )
);

named!(pub object(CompleteStr) -> Object,
    hws!(
        do_parse!(
            char!('{')                                        >>
            list: many0!(terminated!(objectelem, char!(','))) >>
            last: opt!(objectelem)                            >>
            char!('}')                                        >>
            ({
                let mut list = list;
                if let Some(item) = last {
                    list.push(item);
                }
                list
            })
        )
    )
);

named!(pub expression(CompleteStr) -> Expression,
    alt!(
        exprterm => { |et| Expression::ExprTerm(et) }
    )
);

named!(pub exprterm(CompleteStr) -> ExprTerm,
    alt!(
        literalvalue    => { |lv| ExprTerm::LiteralValue(lv) } |
        collectionvalue => { |cv| ExprTerm::CollectionValue(cv) }
    )
);

named!(pub collectionvalue(CompleteStr) -> CollectionValue,
    hws!(
        alt!(
            tuple  => { |t| CollectionValue::Tuple(t) } |
            object => { |o| CollectionValue::Object(o) }
        )
    )
);

named!(pub quoted_template(CompleteStr) -> TemplateExpr,
    hws!(
        flat_map!(
            delimited!(
                char!('"'),
                take_until!(r#"""#),
                char!('"')
            ),
            parse_to!(TemplateExpr)
        )
    )
);

/// todo: implement template trimming
named!(pub heredoc_template(CompleteStr) -> TemplateExpr,
    hws!(
        do_parse!(
            tag!("<<")                   >>
            _do_trim: opt!(char!('-'))   >>
            ident: identifier            >>
            call!(nom::line_ending)      >>
            s: take_until!(&ident.0[..]) >>
            (TemplateExpr(s.to_string()))
        )
    )
);

named!(pub template_expr(CompleteStr) -> TemplateExpr,
    alt!(quoted_template | heredoc_template)
);

named!(pub stringlit(CompleteStr) -> StringLit,
    hws!(
        delimited!(
            char!('"'),
            map!(
                escaped_transform!(
                    is_not!("\\\"\n"),
                    '\\',
                    alt!(
                        tag!("\\") => { |_| &"\\"[..] } |
                        tag!("\"") => { |_| &"\""[..] } |
                        tag!("n")  => { |_| &"\n"[..] }
                    )
                ),
                |s: String| { StringLit(s) }
            ),
            char!('"')
        )
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_comment() {
        let tests = vec![
            (
                "# This is a single line comment\r\n",
                Comment(" This is a single line comment".to_string()),
            ),
            (
                "// This is a single line comment\r\n",
                Comment(" This is a single line comment".to_string()),
            ),
            (
                "// This is a single line comment\n",
                Comment(" This is a single line comment".to_string()),
            ),
            (
                "// This is a single line comment",
                Comment(" This is a single line comment".to_string()),
            ),
            (
                "/* This is a multi\n line\r\n comment */",
                Comment(" This is a multi\n line\r\n comment ".to_string()),
            ),
        ];

        for (text, expected) in tests {
            let actual = comment(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_identifier() {
        let tests = vec![
            ("ident", Identifier("ident".to_string())),
            ("kebab-case", Identifier("kebab-case".to_string())),
            ("snake_case", Identifier("snake_case".to_string())),
            (
                "SCREAMING_SNAKE_CASE",
                Identifier("SCREAMING_SNAKE_CASE".to_string()),
            ),
            ("CamelCase", Identifier("CamelCase".to_string())),
            (
                "Irate_Camel_Case",
                Identifier("Irate_Camel_Case".to_string()),
            ),
            ("I", Identifier("I".to_string())),
            ("i", Identifier("i".to_string())),
            ("_underprefix", Identifier("_underprefix".to_string())),
            (
                "__underunderprefix",
                Identifier("__underunderprefix".to_string()),
            ),
        ];

        for (text, expected) in tests {
            let actual = identifier(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_numericlit() {
        let tests = vec![
            ("1", NumericLit(f64::from_str("1").unwrap())),
            ("1.1", NumericLit(f64::from_str("1.1").unwrap())),
            (" 1.1", NumericLit(f64::from_str("1.1").unwrap())),
            ("1.1 ", NumericLit(f64::from_str("1.1").unwrap())),
            (" 1.1 ", NumericLit(f64::from_str("1.1").unwrap())),
            //(".1", NumericLit(f64::from_str(".1").unwrap())),
            ("1.1e5", NumericLit(f64::from_str("1.1e5").unwrap())),
        ];

        for (text, expected) in tests {
            let actual = numericlit(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_blocklabels() {
        let tests = vec![
            ("", BlockLabels::new()),
            ("ident1", vec![Identifier("ident1".to_string())]),
            (
                "ident1 ident2 ident3",
                vec![
                    Identifier("ident1".to_string()),
                    Identifier("ident2".to_string()),
                    Identifier("ident3".to_string()),
                ],
            ),
        ];

        for (text, expected) in tests {
            let actual = blocklabels(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_exprterm() {
        let tests = vec![(
            "{foo = true, bar = false}",
            ExprTerm::CollectionValue(CollectionValue::Object(vec![
                ObjectElem {
                    key: ObjectKey::Identifier(Identifier("foo".to_string())),
                    value: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                },
                ObjectElem {
                    key: ObjectKey::Identifier(Identifier("bar".to_string())),
                    value: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::False)),
                },
            ])),
        )];

        for (text, expected) in tests {
            let actual = exprterm(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_literalvalue() {
        let tests = vec![
            ("true", LiteralValue::True),
            ("false", LiteralValue::False),
            ("null", LiteralValue::Null),
            ("3.14", LiteralValue::NumericLit(NumericLit(3.14))),
        ];

        for (text, expected) in tests {
            let actual = literalvalue(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_tuple() {
        let tests = vec![
            ("[]", Tuple::new()),
            (
                "[true]",
                vec![Expression::ExprTerm(ExprTerm::LiteralValue(
                    LiteralValue::True,
                ))],
            ),
            (
                "[true, false, null, 3.14]",
                vec![
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::False)),
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::Null)),
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                        NumericLit(3.14),
                    ))),
                ],
            ),
        ];

        for (text, expected) in tests {
            let actual = tuple(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_object() {
        let tests = vec![
            ("{}", Object::new()),
            (
                "{foo = true, bar = false}",
                vec![
                    ObjectElem {
                        key: ObjectKey::Identifier(Identifier("foo".to_string())),
                        value: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                    },
                    ObjectElem {
                        key: ObjectKey::Identifier(Identifier("bar".to_string())),
                        value: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::False)),
                    },
                ],
            ),
        ];

        for (text, expected) in tests {
            let actual = object(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }

    #[test]
    fn test_collectionvalue() {
        let tests = vec![
            ("[]", CollectionValue::Tuple(Tuple::new())),
            ("{}", CollectionValue::Object(Object::new())),
            (
                "[true, false]",
                CollectionValue::Tuple(vec![
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::False)),
                ]),
            ),
            (
                "{foo = true, bar = false}",
                CollectionValue::Object(vec![
                    ObjectElem {
                        key: ObjectKey::Identifier(Identifier("foo".to_string())),
                        value: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                    },
                    ObjectElem {
                        key: ObjectKey::Identifier(Identifier("bar".to_string())),
                        value: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::False)),
                    },
                ]),
            ),
        ];

        for (text, expected) in tests {
            let actual = collectionvalue(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }
}
