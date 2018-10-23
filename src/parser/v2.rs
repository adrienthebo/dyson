use ast;
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
    let out = input.split_at_position(|item| {
        let ch = item.as_char();
        !(ch.is_xid_continue() || ch == '-')
    });
    out
}

named!(identifier(CompleteStr) -> ast::Identifier,
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
        parse_to!(ast::Identifier)
    )
);

named!(numericlit(CompleteStr) -> ast::NumericLit,
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
        parse_to!(ast::NumericLit)
    )
);

named!(pub attribute(CompleteStr) -> ast::Attribute,
    map!(
        separated_pair!(
            identifier,
            char!('='),
            identifier
        ),
        |(ident, _expr)| { ast::Attribute { ident, expr: ast::Expression::ExprTerm } }
    )
);

named!(pub blocklabels(CompleteStr) -> ast::BlockLabels,
    many0!(identifier)
);

named!(pub block(CompleteStr) -> ast::Block,
    do_parse!(
        ident: identifier                               >>
        labels: blocklabels                             >>
        _bodyopen: pair!(char!('{'), nom::line_ending)  >>
        inner: body                                     >>
        _bodyclose: pair!(char!('}'), nom::line_ending) >>
        (ast::Block { ident, labels, body: inner })
    )
);

named!(pub bodyitem(CompleteStr) -> ast::BodyItem,
    alt!(
        attribute => { |a| ast::BodyItem::AttrItem(a) } |
        block     => { |b| ast::BodyItem::BlockItem(b) }
    )
);

named!(pub body(CompleteStr) -> ast::Body,
    map!(
        fold_many0!(bodyitem, ast::BodyItems::new(), |mut acc: ast::BodyItems, it: ast::BodyItem| {
            acc.push(it);
            acc
        }),
        |v: ast::BodyItems| { ast::Body(v) }
    )
);

named!(pub literalvalue(CompleteStr) -> ast::LiteralValue,
    hws!(
        alt!(
            numericlit    => { |nl| ast::LiteralValue::NumericLit(nl) } |
            tag!("true")  => { |_| ast::LiteralValue::True } |
            tag!("false") => { |_| ast::LiteralValue::False } |
            tag!("null")  => { |_| ast::LiteralValue::Null }
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
            (
                "__underunderprefix",
                ast::Identifier("__underunderprefix".to_string()),
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
            ("1", ast::NumericLit(f64::from_str("1").unwrap())),
            ("1.1", ast::NumericLit(f64::from_str("1.1").unwrap())),
            (" 1.1", ast::NumericLit(f64::from_str("1.1").unwrap())),
            ("1.1 ", ast::NumericLit(f64::from_str("1.1").unwrap())),
            (" 1.1 ", ast::NumericLit(f64::from_str("1.1").unwrap())),
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

    #[test]
    fn test_blocklabels() {
        let tests = vec![
            ("", ast::BlockLabels::new()),
            ("ident1", vec![ast::Identifier("ident1".to_string())]),
            (
                "ident1 ident2 ident3",
                vec![
                    ast::Identifier("ident1".to_string()),
                    ast::Identifier("ident2".to_string()),
                    ast::Identifier("ident3".to_string()),
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
    fn test_literalvalue() {
        let tests = vec![
            ("true", ast::LiteralValue::True),
            ("false", ast::LiteralValue::False),
            ("null", ast::LiteralValue::Null),
            ("3.14", ast::LiteralValue::NumericLit(ast::NumericLit(3.14))),
        ];

        for (text, expected) in tests {
            let actual = literalvalue(text.into());
            let (remaining, parsed) = actual.expect("Parse failure");
            assert!(remaining.is_empty());
            assert_eq!(expected, parsed);
        }
    }
}
