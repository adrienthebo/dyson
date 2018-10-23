use ast;
use nom::types::CompleteStr;
use nom::{AsChar, IResult, InputTakeAtPosition};
use std::str::FromStr;

named!(pub multi_line_comment(CompleteStr) -> ast::Comment,
    preceded!(
        tag!("/*"),
        flat_map!(
            take_until_and_consume!("*/"),
            parse_to!(ast::Comment)
        )
    )
);

named!(pub single_line_comment(CompleteStr) -> ast::Comment,
    do_parse!(
        _prefix: alt!(tag!("#") | tag!("//"))        >>
        s: take_till!(|ch| ch == '\r' || ch == '\n') >>
        _suffix: opt!(call!(nom::line_ending))       >>
        (ast::Comment(s.to_string()))
    )
);

named!(pub comment(CompleteStr) -> ast::Comment,
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
    let out = input.split_at_position(
        |item| {
            let ch = item.as_char();
            !(ch.is_xid_continue() || ch == '-')
        }
    );
    out
}

named!(identifier(CompleteStr) -> ast::Identifier,
    flat_map!(
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
        ),
        parse_to!(ast::Identifier)
    )
);

named!(numericlit(CompleteStr) -> ast::NumericLit,
    flat_map!(
        recognize!(
            tuple!(
                nom::digit,
                opt!(preceded!(char!('.'), nom::digit)),
                opt!(preceded!(char!('e'), nom::digit))
            )
        ),
        parse_to!(ast::NumericLit)
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_comment() {
        let tests = vec![
            (
                "# This is a single line comment\r\n",
                ast::Comment(" This is a single line comment".to_string()),
            ),
            (
                "// This is a single line comment\r\n",
                ast::Comment(" This is a single line comment".to_string()),
            ),
            (
                "// This is a single line comment\n",
                ast::Comment(" This is a single line comment".to_string()),
            ),
            (
                "// This is a single line comment",
                ast::Comment(" This is a single line comment".to_string()),
            ),
            (
                "/* This is a multi\n line\r\n comment */",
                ast::Comment(" This is a multi\n line\r\n comment ".to_string()),
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
            ("ident", ast::Identifier("ident".to_string())),
            ("kebab-case", ast::Identifier("kebab-case".to_string())),
            ("snake_case", ast::Identifier("snake_case".to_string())),
            (
                "SCREAMING_SNAKE_CASE",
                ast::Identifier("SCREAMING_SNAKE_CASE".to_string()),
            ),
            ("CamelCase", ast::Identifier("CamelCase".to_string())),
            (
                "Irate_Camel_Case",
                ast::Identifier("Irate_Camel_Case".to_string()),
            ),
            ("I", ast::Identifier("I".to_string())),
            ("i", ast::Identifier("i".to_string())),
            ("_underprefix", ast::Identifier("_underprefix".to_string())),
            ("__underunderprefix", ast::Identifier("__underunderprefix".to_string())),
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
            ("1", ast::NumericLit(f64::from_str("1").unwrap())),
            ("1.1", ast::NumericLit(f64::from_str("1.1").unwrap())),
            //(".1", ast::NumericLit(f64::from_str(".1").unwrap())),
            ("1.1e5", ast::NumericLit(f64::from_str("1.1e5").unwrap())),
        ];

        for (text, expected) in tests {
            let actual = numericlit(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }
}
