//! Convenience macros for logging and introspecting HCL2 parse results.

#[macro_export]
macro_rules! interrobang {
    ($text:expr) => {
        println!("########################################");
        println!("-- text:");
        println!("<<<\n{}\n>>>", $text);
    };

    ($text:expr, $result:expr) => {
        interrobang!($text);

        println!("-- parse result:");
        match &$result {
            Ok((ref remaining, ref ast)) => {
                println!("Parse ok!");
                println!("-- remaining:");
                println!("<<<{:#?}>>>", remaining.0);
                println!("-- ast:");
                println!("{:#?}", ast);
            },
            Err(ref e) => {
                println!("Parse failed, error:");
                println!("<<<\n{:#?}\n>>>", e);
            }
        }
    }
}

/// Fully parse a string, but don't check the resulting AST.
///
/// ```
/// # #[macro_use] extern crate hcl_parser;
///
/// use hcl_parser::parser::v2::parse;
///
/// fn main() {
///     let text =  "foo = true\n";
///     try_recognize!(parse, text);
/// }
/// ```
#[macro_export]
macro_rules! try_recognize {
    ($func:ident, $text:expr) => {
        let result = $func($text.into());
        interrobang!($text, result);
        let (remaining, ast) = result.expect("Parse failure");
        assert!(remaining.is_empty());
    }
}

/// Fully parse a string into a specific AST - or `panic` while trying.
///
/// ```
/// # #[macro_use] extern crate hcl_parser;
///
/// use hcl_parser::parser::v2::*;
/// use hcl_parser::ast::*;
///
/// fn main() {
///     let text = "foo = true\n";
///     let expected = Body(
///         vec![
///             Attribute {
///                 ident: "foo".into(),
///                 expr: Expression::ExprTerm(true.into()),
///             }.into()
///         ]
///     );
///
///     try_parse_to!(parse, text, expected);
/// }
/// ```
#[macro_export]
macro_rules! try_parse_to {
    ($func:ident, $text:expr, $expected:expr) => {
        let result = $func($text.into());
        interrobang!($text, result);
        let (remaining, ast) = result.expect("Parse failure");
        assert!(remaining.is_empty());
        assert_eq!($expected, ast);
    }
}
