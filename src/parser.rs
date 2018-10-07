use std::str::FromStr;
use nom::types::CompleteStr;


named!(parse_string<CompleteStr, String>,
       ws!(
           preceded!(
               char!('"'),
               map_res!(
                   take_until_and_consume!("\""),
                   |s: CompleteStr| { FromStr::from_str(s.0) }
                )
            )
        )
);

named!(parse_multi_line_comment<CompleteStr, String>,
       delimited!(
           tag!("/*"),
            map_res!(take_until!("*/"), |s: CompleteStr| { FromStr::from_str(s.0) }),
            tag!("*/")
        )
);

named!(parse_single_line_comment<CompleteStr, String>,
       delimited!(
           alt!(tag!("//") | tag!("#")),
            map_res!(take_until!("\n"), |s: CompleteStr| { FromStr::from_str(s.0) }),
            tag!("\n")
        )
);

named!(parse_boolean<CompleteStr, bool>,
       alt!(
           tag!("true")  => { |_| true  } |
           tag!("false") => { |_| false }
       )
);

pub mod hcl {
    pub mod ast {
        #[derive(Debug, PartialEq)]
        pub enum Node {
            TFString(String),
            Boolean(bool),
        }
    }
}

named!(parse<CompleteStr, hcl::ast::Node>,
       alt!(
           parse_boolean => { |b| hcl::ast::Node::Boolean(b) } |
           parse_string  => { |s| hcl::ast::Node::TFString(s) }
       )
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_boolean_true() {
        let text = CompleteStr("true");

        let parsed = parse_boolean(text).expect("Parse failed");
        assert_eq!(parsed, (CompleteStr(""), true));
    }

    #[test]
    fn test_boolean_false() {
        let text = CompleteStr("false");

        let parsed = parse_boolean(text).expect("Parse failed");
        assert_eq!(parsed, (CompleteStr(""), false));
    }

    #[test]
    fn test_boolean_none() {
        let text = CompleteStr("ish?");

        parse_boolean(text).expect_err("Parse should have failed");
    }

    #[test]
    fn test_single_line_comment() {
        let pairs = vec![
            ("//\n", ""),
            ("#\n", ""),
            ("//comment1\n", "comment1"),
            ("#comment2\n", "comment2"),
            ("// comment3\n", " comment3"),
            ("# comment4\n", " comment4")
        ];

        for (text, expected) in pairs {
            let parsed = parse_single_line_comment(CompleteStr(text));

            assert_eq!(
                parsed,
                Ok(
                    (CompleteStr(""), expected.to_string())
                )
            );
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
            let parsed = parse_multi_line_comment(CompleteStr(text));

            assert_eq!(
                parsed,
                Ok(
                    (CompleteStr(""), expected.to_string())
                )
            );
        }
    }

    #[test]
    fn test_string() {
        let pairs = vec![
            (r#""""#, ""),
            (r#""hi""#, "hi"),
            (r#""this is more complex!""#, "this is more complex!"),
            ("\"this has a \nnewline!\"", "this has a \nnewline!"),
            (r#" "this has surrounding whitespace!" "#, "this has surrounding whitespace!"),

            // TODO
            // (r#""this has an \" escaped quote!""#, "this has an \" escaped quote!"),
        ];

        for (text, expected) in pairs {
            let parsed = parse_string(CompleteStr(text));

            assert_eq!(
                parsed,
                Ok(
                    (CompleteStr(""), expected.to_string())
                )
            );
        }
    }

    #[test]
    fn test_parse() {
        let pairs = vec![
            (
                r#""""#,
                hcl::ast::Node::TFString("".to_string())
            ),
            (
                r#""hi""#,
                hcl::ast::Node::TFString("hi".to_string())
            ),
            (
                r#""this is more complex!""#, hcl::ast::Node::TFString("this is more complex!".to_string())
            ),
            (
                "\"this has a \nnewline!\"",
                hcl::ast::Node::TFString("this has a \nnewline!".to_string())
            ),
            (
                r#" "this has surrounding whitespace!" "#,
                hcl::ast::Node::TFString("this has surrounding whitespace!".to_string())
            ),
        ];

        for (text, expected) in pairs {
            let parsed = parse(CompleteStr(text));

            assert_eq!(parsed, Ok((CompleteStr(""), expected)));
        }
    }
}
