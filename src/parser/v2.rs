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
                Ok((ltrim, o)) => {
                    match (hsp)(ltrim) {
                        Err(e) => Err(Err::convert(e)),
                        Ok((ztrim, _)) => Ok((ztrim, o))
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

named!(variable_expr(CompleteStr) -> VariableExpr,
    flat_map!(
        recognize!(identifier),
        parse_to!(VariableExpr)
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
        collectionvalue => { |cv| ExprTerm::CollectionValue(cv) } |
        template_expr   => { |t| ExprTerm::TemplateExpr(t) } |
        functioncall    => { |f| ExprTerm::FunctionCall(f) } |
        variable_expr   => { |v| ExprTerm::VariableExpr(v) }
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

named!(pub functioncall(CompleteStr) -> FunctionCall,
    map!(
        pair!(
            identifier,
            delimited!(
                char!('('),
                pair!(
                    many0!(terminated!(expression, char!(','))),
                    opt!(expression)
                ),
                char!(')')
            )
        ),
        |(ident, (rest, last))| {
            let mut arguments = rest;
            if let Some(item) = last {
                arguments.push(item);
            }
            FunctionCall { ident, arguments }
        }
    )
);

named!(for_cond(CompleteStr) -> ForCond,
    map!(
        preceded!(
            tag!("if"),
            expression
        ),
        |e: Expression| { ForCond(e) }
    )
);

named!(for_intro(CompleteStr) -> ForIntro,
    do_parse!(
        tag!("for")                                                        >>
        idents: pair!(identifier, opt!(preceded!(char!(','), identifier))) >>
        tag!("in")                                                         >>
        expr: expression                                                   >>
        char!(':')                                                         >>
        (ForIntro { idents, expr })
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    macro_rules! test_production {
        ($testname:ident, $func:ident, $cases:expr) => {
            #[test]
            fn $testname() {
                for (text, expected) in $cases {
                    let actual = $func(text.into());
                    let (remaining, parsed) = actual.expect("Parse failure");
                    assert!(remaining.is_empty());
                    assert_eq!(expected, parsed);
                }
            }
        };
    }

    test_production!(
        test_comment,
        comment,
        vec![
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
        ]
    );

    test_production!(
        test_identifier,
        identifier,
        vec![
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
        ]
    );

    test_production!(
        test_numericlit,
        numericlit,
        vec![
            ("1", NumericLit(f64::from_str("1").unwrap())),
            ("1.1", NumericLit(f64::from_str("1.1").unwrap())),
            (" 1.1", NumericLit(f64::from_str("1.1").unwrap())),
            ("1.1 ", NumericLit(f64::from_str("1.1").unwrap())),
            (" 1.1 ", NumericLit(f64::from_str("1.1").unwrap())),
            //(".1", NumericLit(f64::from_str(".1").unwrap())),
            ("1.1e5", NumericLit(f64::from_str("1.1e5").unwrap())),
        ]
    );

    test_production!(
        test_blocklabels,
        blocklabels,
        vec![
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
        ]
    );

    test_production!(
        test_exprterm,
        exprterm,
        vec![
            (
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
            ),
            (
                "i-am-a-variable",
                ExprTerm::VariableExpr(VariableExpr("i-am-a-variable".to_string())),
            ),
            (
                "func(true)",
                ExprTerm::FunctionCall(FunctionCall {
                    ident: Identifier("func".into()),
                    arguments: vec![Expression::ExprTerm(ExprTerm::LiteralValue(
                        LiteralValue::True
                    )),],
                })
            )
        ]
    );

    test_production!(
        test_literalvalue,
        literalvalue,
        vec![
            ("true", LiteralValue::True),
            ("false", LiteralValue::False),
            ("null", LiteralValue::Null),
            ("3.14", LiteralValue::NumericLit(NumericLit(3.14))),
        ]
    );

    test_production!(
        test_tuple,
        tuple,
        vec![
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
        ]
    );

    test_production!(
        test_object,
        object,
        vec![
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
        ]
    );

    test_production!(
        test_collectionvalue,
        collectionvalue,
        vec![
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
        ]
    );

    test_production!(
        test_functioncall,
        functioncall,
        vec![
            (
                "nullary()",
                FunctionCall {
                    ident: Identifier("nullary".into()),
                    arguments: vec![],
                },
            ),
            (
                "unary(true)",
                FunctionCall {
                    ident: Identifier("unary".into()),
                    arguments: vec![
                        // Read this type declaration to the tune of yakety sax
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                    ],
                },
            ),
            (
                "binary(true, false)",
                FunctionCall {
                    ident: Identifier("binary".into()),
                    arguments: vec![
                        // Read this type declaration to the tune of yakety sax
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::False)),
                    ],
                },
            ),
            (
                "ternary(true, false, null)",
                FunctionCall {
                    ident: Identifier("ternary".into()),
                    arguments: vec![
                        // Read this type declaration to the tune of yakety sax
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True)),
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::False)),
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::Null)),
                    ],
                },
            ),
        ]
    );

    test_production!(
        test_for_cond,
        for_cond,
        vec![
            (
                "if true",
                ForCond(Expression::ExprTerm(ExprTerm::LiteralValue(
                    LiteralValue::True
                )),)
            ),
            (
                "if false",
                ForCond(Expression::ExprTerm(ExprTerm::LiteralValue(
                    LiteralValue::False
                )),)
            ),
        ]
    );

    test_production!(
        test_for_intro,
        for_intro,
        vec![(
            "for item in [1, 2, 3]:",
            ForIntro {
                idents: (Identifier("item".to_string()), None),
                expr: Expression::ExprTerm(ExprTerm::CollectionValue(CollectionValue::Tuple(
                    vec![
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                            NumericLit(1.0)
                        ))),
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                            NumericLit(2.0)
                        ))),
                        Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                            NumericLit(3.0)
                        )))
                    ]
                )))
            }
        )]
    );
}
