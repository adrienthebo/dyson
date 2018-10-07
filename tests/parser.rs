extern crate hcl_parser;
extern crate nom;

use hcl_parser::parser;
use nom::types::CompleteStr;

#[test]
fn test_boolean_true() {
    let text = CompleteStr("true");

    let parsed = parser::parse_boolean(text).expect("Parse failed");
    assert_eq!(parsed, (CompleteStr(""), true));
}

#[test]
fn test_boolean_false() {
    let text = CompleteStr("false");

    let parsed = parser::parse_boolean(text).expect("Parse failed");
    assert_eq!(parsed, (CompleteStr(""), false));
}

#[test]
fn test_boolean_none() {
    let text = CompleteStr("ish?");

    parser::parse_boolean(text).expect_err("Parse should have failed");
}

#[test]
fn test_single_line_comment() {
    let pairs = vec![
        ("//\n", ""),
        ("#\n", ""),
        ("//comment1\n", "comment1"),
        ("#comment2\n", "comment2"),
        ("// comment3\n", " comment3"),
        ("# comment4\n", " comment4"),
    ];

    for (text, expected) in pairs {
        let parsed = parser::parse_single_line_comment(CompleteStr(text));

        assert_eq!(parsed, Ok((CompleteStr(""), expected.to_string())));
    }
}

#[test]
fn test_multi_line_comment() {
    let pairs = vec![
        ("/**/", ""),
        ("/***/", "*"),
        ("/* comment 1 */", " comment 1 "),
    ];

    for (text, expected) in pairs {
        let parsed = parser::parse_multi_line_comment(CompleteStr(text));

        assert_eq!(parsed, Ok((CompleteStr(""), expected.to_string())));
    }
}

#[test]
fn test_string() {
    let pairs = vec![
        (r#""""#, ""),
        (r#""hi""#, "hi"),
        (r#""this is more complex!""#, "this is more complex!"),
        ("\"this has a \nnewline!\"", "this has a \nnewline!"),
        (
            r#" "this has surrounding whitespace!" "#,
            "this has surrounding whitespace!",
        ),
        // TODO
        // (r#""this has an \" escaped quote!""#, "this has an \" escaped quote!"),
    ];

    for (text, expected) in pairs {
        let parsed = parser::parse_string(CompleteStr(text));

        assert_eq!(parsed, Ok((CompleteStr(""), expected.to_string())));
    }
}

#[test]
fn test_integer() {
    let pairs = vec![
        ("0", 0),
        (" 1", 1),
        ("0x01 ", 1),
        ("0x10", 16),
        ("01 ", 1),
        (" 010 ", 8),
    ];

    for (text, expected) in pairs {
        let parsed = parser::parse_integer(CompleteStr(text));

        assert_eq!(parsed, Ok((CompleteStr(""), expected)));
    }
}

#[test]
fn test_parse() {
    let pairs = vec![
        (r#""""#, hcl_parser::ast::Node::TFString("".to_string())),
        (r#""hi""#, hcl_parser::ast::Node::TFString("hi".to_string())),
        (
            r#""this is more complex!""#,
            hcl_parser::ast::Node::TFString("this is more complex!".to_string()),
        ),
        (
            "\"this has a \nnewline!\"",
            hcl_parser::ast::Node::TFString("this has a \nnewline!".to_string()),
        ),
        (
            r#" "this has surrounding whitespace!" "#,
            hcl_parser::ast::Node::TFString("this has surrounding whitespace!".to_string()),
        ),
        ("0", hcl_parser::ast::Node::TFInteger(0)),
        (" 1", hcl_parser::ast::Node::TFInteger(1)),
        ("0x01 ", hcl_parser::ast::Node::TFInteger(1)),
        ("0x10", hcl_parser::ast::Node::TFInteger(16)),
        ("01 ", hcl_parser::ast::Node::TFInteger(1)),
        (" 010 ", hcl_parser::ast::Node::TFInteger(8)),
        ("0.5", hcl_parser::ast::Node::TFFloat(0.5)),
        ("5.5", hcl_parser::ast::Node::TFFloat(5.5)),
        ("[]", hcl_parser::ast::Node::Array(Vec::new())),
        (
            "[1, 01, 0x01]",
            hcl_parser::ast::Node::Array(vec![
                hcl_parser::ast::Node::TFInteger(1),
                hcl_parser::ast::Node::TFInteger(1),
                hcl_parser::ast::Node::TFInteger(1),
            ]),
        ),
        (
            r#"[1, "this is a complex type"]"#,
            hcl_parser::ast::Node::Array(vec![
                hcl_parser::ast::Node::TFInteger(1),
                hcl_parser::ast::Node::TFString("this is a complex type".to_string()),
            ]),
        ),
    ];

    for (text, expected) in pairs {
        let parsed = parser::parse(CompleteStr(text));

        assert_eq!(parsed, Ok((CompleteStr(""), expected)));
    }
}
