extern crate hcl_parser;
#[macro_use]
extern crate pretty_assertions;

use hcl_parser::ast::*;
use hcl_parser::parser::v2::*;

macro_rules! test_production {
    ($testname:ident, $func:ident, $cases:expr) => {
        #[test]
        fn $testname() {
            for (text, expected) in $cases {
                println!("--------------------------------------------------------------------------------");
                println!("-- text:");
                println!("<<<\n{:#?}\n>>>", text);

                let actual = $func(text.into());
                let (remaining, ast) = actual.expect("Parse failure");

                println!("-- remaining:");
                println!("<<<\n{:#?}\n>>>", remaining);
                println!("-- ast:");
                println!("{:#?}", ast);
                println!("-- expected:");
                println!("<<<\n{:#?}\n>>>", expected);

                assert!(remaining.is_empty());
                assert_eq!(expected, ast);
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
        ("ident", Identifier::from("ident")),
        ("kebab-case", Identifier::from("kebab-case")),
        ("snake_case", Identifier::from("snake_case")),
        (
            "SCREAMING_SNAKE_CASE",
            Identifier::from("SCREAMING_SNAKE_CASE"),
        ),
        ("CamelCase", Identifier::from("CamelCase")),
        ("Irate_Camel_Case", Identifier::from("Irate_Camel_Case"),),
        ("I", Identifier::from("I")),
        ("i", Identifier::from("i")),
        ("_underprefix", Identifier::from("_underprefix")),
        ("__underunderprefix", Identifier::from("__underunderprefix"),),
        (" left-whitespace", Identifier::from("left-whitespace"),),
        ("right-whitespace ", Identifier::from("right-whitespace"),),
        (" both-whitespace ", Identifier::from("both-whitespace"),),
    ]
);

test_production!(
    test_numericlit,
    numericlit,
    vec![
        ("1", NumericLit(1_f64)),
        ("1.1", NumericLit(1.1_f64)),
        (" 1.1", NumericLit(1.1_f64)),
        ("1.1 ", NumericLit(1.1_f64)),
        (" 1.1 ", NumericLit(1.1_f64)),
        ("1.1e5", NumericLit(1.1e5_f64)),
    ]
);

test_production!(
    test_blocklabels,
    blocklabels,
    vec![
        ("", BlockLabels::new()),
        ("ident1", vec![BlockLabel::Identifier("ident1".into())]),
        (
            "ident1 ident2 ident3",
            vec![
                BlockLabel::Identifier("ident1".into()),
                BlockLabel::Identifier("ident2".into()),
                BlockLabel::Identifier("ident3".into()),
            ],
        ),
        (
            r#"ident "stringlit""#,
            vec![
                BlockLabel::Identifier("ident".into()),
                BlockLabel::StringLit("stringlit".into()),
            ],
        ),
    ]
);

test_production!(
    test_attribute,
    attribute,
    vec![
        (
            "foo = \"bar\"\n",
            Attribute {
                ident: "foo".into(),
                expr: Expression::ExprTerm(ExprTerm::TemplateExpr(TemplateExpr::from("bar")))
            },
        ),
        (
            "obj = {hello = \"world\"}\n",
            Attribute {
                ident: "obj".into(),
                expr: Expression::ExprTerm(ExprTerm::CollectionValue(CollectionValue::Object(
                    vec![ObjectElem {
                        key: ObjectKey::Identifier("hello".into()),
                        value: Expression::ExprTerm(ExprTerm::TemplateExpr(TemplateExpr::from(
                            "world"
                        )))
                    }]
                )))
            },
        )
    ]
);

test_production!(
    test_block,
    block,
    vec![
        (
            "nullaryblock {\n  blockitem = true\n}\n",
            Block {
                ident: "nullaryblock".into(),
                labels: vec![],
                body: Body(vec![BodyItem::AttrItem(Attribute {
                    ident: "blockitem".into(),
                    expr: Expression::ExprTerm(true.into())
                })])
            }
        ),
        (
            "unaryblock \"stringlit\" {\n  blockitem = true\n}\n",
            Block {
                ident: "unaryblock".into(),
                labels: vec![BlockLabel::StringLit("stringlit".into())],
                body: Body(vec![BodyItem::AttrItem(Attribute {
                    ident: "blockitem".into(),
                    expr: Expression::ExprTerm(true.into())
                })])
            }
        ),
        (
            "binaryblock \"stringlit\" ident1 {\n  blockitem = true\n}\n",
            Block {
                ident: "binaryblock".into(),
                labels: vec![
                    BlockLabel::StringLit("stringlit".into()),
                    BlockLabel::Identifier("ident1".into())
                ],
                body: Body(vec![BodyItem::AttrItem(Attribute {
                    ident: "blockitem".into(),
                    expr: Expression::ExprTerm(true.into())
                })])
            }
        ),
    ]
);

test_production!(
    test_exprterm,
    exprterm,
    vec![
        (
            "{foo = true, bar = false}",
            CollectionValue::Object(vec![
                ObjectElem {
                    key: ObjectKey::Identifier("foo".into()),
                    value: Expression::ExprTerm(true.into()),
                },
                ObjectElem {
                    key: ObjectKey::Identifier("bar".into()),
                    value: Expression::ExprTerm(false.into()),
                },
            ])
            .into(),
        ),
        (
            "i-am-a-variable",
            ExprTerm::VariableExpr("i-am-a-variable".into()),
        ),
        (
            " left-whitespace",
            ExprTerm::VariableExpr("left-whitespace".into()),
        ),
        (
            "right-whitespace ",
            ExprTerm::VariableExpr("right-whitespace".into()),
        ),
        (
            "func(true)",
            ExprTerm::FunctionCall(FunctionCall {
                ident: "func".into(),
                arguments: vec![Expression::ExprTerm(true.into()),],
            })
        )
    ]
);

test_production!(
    test_literalvalue,
    literalvalue,
    vec![
        ("true", true.into()),
        ("false", false.into()),
        ("null", LiteralValue::Null),
        ("3.14", 3.14.into())
    ]
);

test_production!(
    test_tuple,
    tuple,
    vec![
        ("[]", Tuple::new()),
        ("[true]", vec![Expression::ExprTerm(true.into())],),
        (
            "[true, false, null, 3.14]",
            vec![
                Expression::ExprTerm(true.into()),
                Expression::ExprTerm(false.into()),
                Expression::ExprTerm(LiteralValue::Null.into()),
                Expression::ExprTerm(3.14.into())
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
                    key: ObjectKey::Identifier("foo".into()),
                    value: Expression::ExprTerm(true.into()),
                },
                ObjectElem {
                    key: ObjectKey::Identifier("bar".into()),
                    value: Expression::ExprTerm(false.into()),
                },
            ],
        ),
        (
            "{foo = true\nbar = false}",
            vec![
                ObjectElem {
                    key: ObjectKey::Identifier("foo".into()),
                    value: Expression::ExprTerm(true.into()),
                },
                ObjectElem {
                    key: ObjectKey::Identifier("bar".into()),
                    value: Expression::ExprTerm(false.into()),
                },
            ],
        ),
        (
            "{\nfoo = true, bar = false\n}",
            vec![
                ObjectElem {
                    key: ObjectKey::Identifier("foo".into()),
                    value: Expression::ExprTerm(true.into()),
                },
                ObjectElem {
                    key: ObjectKey::Identifier("bar".into()),
                    value: Expression::ExprTerm(false.into()),
                },
            ],
        ),
        (
            "{\n  foo = true,\n  bar = false\n}",
            vec![
                ObjectElem {
                    key: ObjectKey::Identifier("foo".into()),
                    value: Expression::ExprTerm(true.into()),
                },
                ObjectElem {
                    key: ObjectKey::Identifier("bar".into()),
                    value: Expression::ExprTerm(false.into()),
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
                Expression::ExprTerm(true.into()),
                Expression::ExprTerm(false.into()),
            ]),
        ),
        (
            "{foo = true, bar = false}",
            CollectionValue::Object(vec![
                ObjectElem {
                    key: ObjectKey::Identifier("foo".into()),
                    value: Expression::ExprTerm(true.into()),
                },
                ObjectElem {
                    key: ObjectKey::Identifier("bar".into()),
                    value: Expression::ExprTerm(false.into()),
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
                ident: "nullary".into(),
                arguments: vec![],
            },
        ),
        (
            "unary(true)",
            FunctionCall {
                ident: "unary".into(),
                arguments: vec![Expression::ExprTerm(true.into()),],
            },
        ),
        (
            "binary(true, false)",
            FunctionCall {
                ident: "binary".into(),
                arguments: vec![
                    Expression::ExprTerm(true.into()),
                    Expression::ExprTerm(false.into()),
                ],
            },
        ),
        (
            "ternary(true, false, null)",
            FunctionCall {
                ident: "ternary".into(),
                arguments: vec![
                    Expression::ExprTerm(true.into()),
                    Expression::ExprTerm(false.into()),
                    Expression::ExprTerm(LiteralValue::Null.into()),
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
            ForCond(Box::new(Expression::ExprTerm(true.into())))
        ),
        (
            "if false",
            ForCond(Box::new(Expression::ExprTerm(false.into())))
        ),
    ]
);

test_production!(
    test_for_intro,
    for_intro,
    vec![(
        "for item in [1, 2, 3]:",
        ForIntro {
            idents: ("item".into(), None),
            expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                CollectionValue::Tuple(vec![
                    Expression::ExprTerm(1.0_f64.into()),
                    Expression::ExprTerm(2.0_f64.into()),
                    Expression::ExprTerm(3.0_f64.into()),
                ])
            )))
        }
    )]
);

test_production!(
    test_for_tuple_expr,
    for_tuple_expr,
    vec![
        (
            "[for item in [1, 2, 3]: item]",
            ForTupleExpr {
                intro: ForIntro {
                    idents: ("item".into(), None),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                        CollectionValue::Tuple(vec![
                            Expression::ExprTerm(1.0_f64.into()),
                            Expression::ExprTerm(2.0_f64.into()),
                            Expression::ExprTerm(3.0_f64.into()),
                        ])
                    )))
                },
                expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("item".into()))),
                cond: None,
            }
        ),
        (
            "[for item in [1, 2, 3]: item if true]",
            ForTupleExpr {
                intro: ForIntro {
                    idents: ("item".into(), None),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                        CollectionValue::Tuple(vec![
                            Expression::ExprTerm(1.0_f64.into()),
                            Expression::ExprTerm(2.0_f64.into()),
                            Expression::ExprTerm(3.0_f64.into()),
                        ])
                    )))
                },
                expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("item".into()))),
                cond: Some(ForCond(Box::new(Expression::ExprTerm(
                    ExprTerm::LiteralValue(true.into(),)
                )))),
            }
        )
    ]
);

test_production!(
    test_for_object_expr,
    for_object_expr,
    vec![
        (
            r#"{for k, v in injective: v => k}"#,
            ForObjectExpr {
                intro: ForIntro {
                    idents: ("k".into(), Some("v".into())),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(
                        "injective".into()
                    )))
                },
                k_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("v".into()))),
                v_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("k".into()))),
                group: false,
                cond: None,
            }
        ),
        (
            r#"{for k, v in {}: v => k}"#,
            ForObjectExpr {
                intro: ForIntro {
                    idents: ("k".into(), Some("v".into())),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                        CollectionValue::Object(vec![])
                    )))
                },
                k_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("v".into()))),
                v_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("k".into()))),
                group: false,
                cond: None,
            }
        )
    ]
);

test_production!(
    test_for_expr,
    for_expr,
    vec![
        (
            r#"{for k, v in injective: v => k}"#,
            ForExpr::ForObjectExpr(ForObjectExpr {
                intro: ForIntro {
                    idents: ("k".into(), Some("v".into())),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                        "injective".to_string()
                    ))))
                },
                k_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("v".into()))),
                v_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("k".into()))),
                group: false,
                cond: None,
            })
        ),
        (
            "[for item in [1, 2, 3]: item]",
            ForExpr::ForTupleExpr(ForTupleExpr {
                intro: ForIntro {
                    idents: ("item".into(), None),
                    expr: Box::new(Expression::ExprTerm(
                        CollectionValue::Tuple(vec![
                            Expression::ExprTerm(1.0_f64.into()),
                            Expression::ExprTerm(2.0_f64.into()),
                            Expression::ExprTerm(3.0_f64.into()),
                        ])
                        .into()
                    ))
                },
                expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr("item".into()))),
                cond: None,
            })
        ),
    ]
);

test_production!(
    test_body,
    body,
    vec![
        (
            "attr = true\n\nblock {\n  blockitem = null\n}\n",
            Body(vec![
                BodyItem::AttrItem(Attribute {
                    ident: "attr".into(),
                    expr: Expression::ExprTerm(true.into())
                }),
                BodyItem::BlockItem(Block {
                    ident: "block".into(),
                    labels: vec![],
                    body: Body(vec![BodyItem::AttrItem(Attribute {
                        ident: "blockitem".into(),
                        expr: Expression::ExprTerm(LiteralValue::Null.into())
                    })])
                })
            ])
        ),
        (
            "attr = true\n\nblock \"stringlit1\" {\n  blockitem = null\n}\n",
            Body(vec![
                BodyItem::AttrItem(Attribute {
                    ident: "attr".into(),
                    expr: Expression::ExprTerm(true.into())
                }),
                BodyItem::BlockItem(Block {
                    ident: "block".into(),
                    labels: vec![BlockLabel::StringLit("stringlit1".into(),)],
                    body: Body(vec![BodyItem::AttrItem(Attribute {
                        ident: "blockitem".into(),
                        expr: Expression::ExprTerm(LiteralValue::Null.into())
                    })])
                })
            ])
        ),
        (
            "attr = true\n\nblock ident1 \"stringlit1\" {\n  blockitem = null\n}\n",
            Body(vec![
                BodyItem::AttrItem(Attribute {
                    ident: "attr".into(),
                    expr: Expression::ExprTerm(true.into(),)
                }),
                BodyItem::BlockItem(Block {
                    ident: "block".into(),
                    labels: vec![
                        BlockLabel::Identifier("ident1".into()),
                        BlockLabel::StringLit("stringlit1".into(),)
                    ],
                    body: Body(vec![BodyItem::AttrItem(Attribute {
                        ident: Identifier("blockitem".to_string()),
                        expr: Expression::ExprTerm(LiteralValue::Null.into())
                    })])
                })
            ])
        ),
        (
            "attr1 = true\nattr2 = false\nattr3 = null\n",
            Body(vec![
                BodyItem::AttrItem(Attribute {
                    ident: "attr1".into(),
                    expr: Expression::ExprTerm(true.into(),)
                }),
                BodyItem::AttrItem(Attribute {
                    ident: "attr2".into(),
                    expr: Expression::ExprTerm(false.into(),)
                }),
                BodyItem::AttrItem(Attribute {
                    ident: "attr3".into(),
                    expr: Expression::ExprTerm(LiteralValue::Null.into())
                }),
            ])
        ),
        (
            "attr1 = true\n\n\nattr2 = false\n\n\n",
            Body(vec![
                BodyItem::AttrItem(Attribute {
                    ident: "attr1".into(),
                    expr: Expression::ExprTerm(true.into(),)
                }),
                BodyItem::AttrItem(Attribute {
                    ident: "attr2".into(),
                    expr: Expression::ExprTerm(false.into(),)
                }),
            ])
        ),
        (
            "foo \"baz\" {\n        key = 7\n        foo = \"bar\"\n}\n",
            Body(vec![BodyItem::BlockItem(Block {
                ident: Identifier("foo".to_string()),
                labels: vec![BlockLabel::StringLit("baz".into()),],
                body: Body(vec![
                    BodyItem::AttrItem(Attribute {
                        ident: Identifier("key".to_string()),
                        expr: Expression::ExprTerm(7.0_f64.into())
                    }),
                    BodyItem::AttrItem(Attribute {
                        ident: Identifier("foo".to_string()),
                        expr: Expression::ExprTerm(ExprTerm::TemplateExpr(TemplateExpr::from(
                            "bar"
                        )))
                    })
                ])
            })])
        ),
        (
            "attr =<<EOD\nhello, world!\nEOD\n",
            Body(vec![BodyItem::AttrItem(Attribute {
                ident: "attr".into(),
                expr: Expression::ExprTerm(ExprTerm::TemplateExpr(
                    TemplateExpr::from("hello, world!\n").into()
                ))
            }),])
        )
    ]
);

test_production!(
    test_templateexpr_heredoc,
    template_expr,
    vec![
        (
            "<<EOD\nhello, world!\nEOD",
            TemplateExpr("hello, world!\n".into())
        ),
        (
            "<<EOD\n  hello,\n  world!\nEOD",
            TemplateExpr("  hello,\n  world!\n".into())
        ),
        (
            "<<-EOD\n  hello,\n   world!\nEOD",
            TemplateExpr("hello,\n world!\n".into())
        ),
        (
            "<<-OVERLY_DESCRIPTIVE_TERMINATOR\n  hello,\n   world!\nOVERLY_DESCRIPTIVE_TERMINATOR",
            TemplateExpr("hello,\n world!\n".into())
        )
    ]
);
