extern crate hcl_parser;

use hcl_parser::ast::*;
use hcl_parser::parser::v2::*;

use std::str::FromStr;

macro_rules! test_production {
    ($testname:ident, $func:ident, $cases:expr) => {
        #[test]
        fn $testname() {
            for (text, expected) in $cases {
                println!("--------------------------------------------------------------------------------");
                println!("-- text:");
                println!("<<<\n{}\n>>>", text);

                let actual = $func(text.into());
                let (remaining, ast) = actual.expect("Parse failure");

                println!("-- ast:");
                println!("{:#?}", ast);
                println!("-- remaining:");
                println!("<<<\n{}\n>>>", remaining);

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
        (
            " left-whitespace",
            Identifier("left-whitespace".to_string()),
        ),
        (
            "right-whitespace ",
            Identifier("right-whitespace".to_string()),
        ),
        (
            " both-whitespace ",
            Identifier("both-whitespace".to_string()),
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
        (
            "ident1",
            vec![BlockLabel::Identifier(Identifier("ident1".to_string()))]
        ),
        (
            "ident1 ident2 ident3",
            vec![
                BlockLabel::Identifier(Identifier("ident1".to_string())),
                BlockLabel::Identifier(Identifier("ident2".to_string())),
                BlockLabel::Identifier(Identifier("ident3".to_string())),
            ],
        ),
        (
            r#"ident "stringlit""#,
            vec![
                BlockLabel::Identifier(Identifier("ident".to_string())),
                BlockLabel::StringLit(StringLit("stringlit".to_string())),
            ],
        ),
    ]
);

test_production!(
    test_block,
    block,
    vec![(
        "nullaryblock {\n  blockitem = true\n}\n",
        Block {
            ident: Identifier("block".to_string()),
            labels: vec![],
            body: Body(vec![BodyItem::AttrItem(Attribute {
                ident: Identifier("blockitem".to_string()),
                expr: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::Null))
            })])
        }
    )]
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
            " left-whitespace",
            ExprTerm::VariableExpr(VariableExpr("left-whitespace".to_string())),
        ),
        (
            "right-whitespace ",
            ExprTerm::VariableExpr(VariableExpr("right-whitespace".to_string())),
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
        (
            "{foo = true\nbar = false}",
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
        (
            "{\nfoo = true, bar = false\n}",
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
        (
            "{\n  foo = true,\n  bar = false\n}",
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
            ForCond(Box::new(Expression::ExprTerm(ExprTerm::LiteralValue(
                LiteralValue::True
            )),))
        ),
        (
            "if false",
            ForCond(Box::new(Expression::ExprTerm(ExprTerm::LiteralValue(
                LiteralValue::False
            )),))
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
            expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                CollectionValue::Tuple(vec![
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                        NumericLit(1.0)
                    ))),
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                        NumericLit(2.0)
                    ))),
                    Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                        NumericLit(3.0)
                    )))
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
                    idents: (Identifier("item".to_string()), None),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                        CollectionValue::Tuple(vec![
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(1.0)
                            ))),
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(2.0)
                            ))),
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(3.0)
                            )))
                        ])
                    )))
                },
                expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "item".to_string()
                )))),
                cond: None,
            }
        ),
        (
            "[for item in [1, 2, 3]: item if true]",
            ForTupleExpr {
                intro: ForIntro {
                    idents: (Identifier("item".to_string()), None),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                        CollectionValue::Tuple(vec![
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(1.0)
                            ))),
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(2.0)
                            ))),
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(3.0)
                            )))
                        ])
                    )))
                },
                expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "item".to_string()
                )))),
                cond: Some(ForCond(Box::new(Expression::ExprTerm(
                    ExprTerm::LiteralValue(LiteralValue::True)
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
                    idents: (
                        Identifier("k".to_string()),
                        Some(Identifier("v".to_string()))
                    ),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                        "injective".to_string()
                    ))))
                },
                k_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "v".to_string()
                )))),
                v_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "k".to_string()
                )))),
                group: false,
                cond: None,
            }
        ),
        (
            r#"{for k, v in {}: v => k}"#,
            ForObjectExpr {
                intro: ForIntro {
                    idents: (
                        Identifier("k".to_string()),
                        Some(Identifier("v".to_string()))
                    ),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                        CollectionValue::Object(vec![])
                    )))
                },
                k_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "v".to_string()
                )))),
                v_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "k".to_string()
                )))),
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
                    idents: (
                        Identifier("k".to_string()),
                        Some(Identifier("v".to_string()))
                    ),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                        "injective".to_string()
                    ))))
                },
                k_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "v".to_string()
                )))),
                v_expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "k".to_string()
                )))),
                group: false,
                cond: None,
            })
        ),
        (
            "[for item in [1, 2, 3]: item]",
            ForExpr::ForTupleExpr(ForTupleExpr {
                intro: ForIntro {
                    idents: (Identifier("item".to_string()), None),
                    expr: Box::new(Expression::ExprTerm(ExprTerm::CollectionValue(
                        CollectionValue::Tuple(vec![
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(1.0)
                            ))),
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(2.0)
                            ))),
                            Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::NumericLit(
                                NumericLit(3.0)
                            )))
                        ])
                    )))
                },
                expr: Box::new(Expression::ExprTerm(ExprTerm::VariableExpr(VariableExpr(
                    "item".to_string()
                )))),
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
                    ident: Identifier("attr".to_string()),
                    expr: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True))
                }),
                BodyItem::BlockItem(Block {
                    ident: Identifier("block".to_string()),
                    labels: vec![],
                    body: Body(vec![BodyItem::AttrItem(Attribute {
                        ident: Identifier("blockitem".to_string()),
                        expr: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::Null))
                    })])
                })
            ])
        ),
        (
            "attr = true\n\nblock \"stringlit1\" {\n  blockitem = null\n}\n",
            Body(
                vec![
                BodyItem::AttrItem(Attribute {
                    ident: Identifier("attr".to_string()),
                    expr: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True))
                }),
                BodyItem::BlockItem(Block {
                    ident: Identifier("block".to_string()),
                    labels: vec![
                        BlockLabel::StringLit(
                            StringLit("stringlit1".into()),
                        )
                    ],
                    body: Body(vec![BodyItem::AttrItem(Attribute {
                        ident: Identifier("blockitem".to_string()),
                        expr: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::Null))
                    })])
                })
            ])
        ),
        (
            "attr = true\n\nblock ident1 \"stringlit1\" {\n  blockitem = null\n}\n",
            Body(
                vec![
                BodyItem::AttrItem(Attribute {
                    ident: Identifier("attr".to_string()),
                    expr: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::True))
                }),
                BodyItem::BlockItem(Block {
                    ident: Identifier("block".to_string()),
                    labels: vec![
                        BlockLabel::Identifier(
                            Identifier("ident1".into())
                        ),
                        BlockLabel::StringLit(
                            StringLit("stringlit1".into()),
                        )
                    ],
                    body: Body(vec![BodyItem::AttrItem(Attribute {
                        ident: Identifier("blockitem".to_string()),
                        expr: Expression::ExprTerm(ExprTerm::LiteralValue(LiteralValue::Null))
                    })])
                })
            ])
        ),
        (
            "foo \"baz\" {\n        key = 7\n        foo = \"bar\n\"\n        \n}",
            Body(
                vec![
                    BodyItem::BlockItem(Block {
                        ident: Identifier("foo".to_string()),
                        labels: vec![
                            BlockLabel::StringLit(
                                StringLit("baz".to_string())
                            ),
                        ],
                        body: Body(
                            vec![
                                BodyItem::AttrItem(
                                    Attribute {
                                        ident: Identifier("key".to_string()),
                                        expr: Expression::ExprTerm(
                                            ExprTerm::LiteralValue(
                                                LiteralValue::NumericLit(
                                                    NumericLit(7.0)
                                                )
                                            )
                                        )
                                    }
                                )
                            ]
                        )
                    }
                )
            ]
        )
    )
    ]
);
