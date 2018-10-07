use nom::types::CompleteStr;
use std::str::FromStr;

use super::ast;

named!(pub parse_string<CompleteStr, String>,
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

named!(pub parse_multi_line_comment<CompleteStr, String>,
       preceded!(
           tag!("/*"),
            map_res!(
                take_until_and_consume!("*/"),
                |s: CompleteStr| { FromStr::from_str(s.0) }
            )
        )
);

named!(pub parse_single_line_comment<CompleteStr, String>,
       preceded!(
           alt!(tag!("//") | tag!("#")),
            map_res!(
                take_until_and_consume!("\n"),
                |s: CompleteStr| { FromStr::from_str(s.0) }
            )
        )
);

named!(pub parse_hex_int<CompleteStr, i64>,
       map_res!( preceded!(tag!("0x"), nom::hex_digit1), |s:  CompleteStr| {  i64::from_str_radix(s.0, 16) })
);

named!(pub parse_oct_int<CompleteStr, i64>,
       map_res!( preceded!(tag!("0"), nom::oct_digit1), |s:  CompleteStr| {  i64::from_str_radix(s.0, 8) }
        )
);

named!(pub parse_dec_int<CompleteStr, i64>,
       map_res!(nom::digit1, |s:  CompleteStr| {  i64::from_str_radix(s.0, 10) })
);

named!(pub parse_integer<CompleteStr, i64>,
       ws!(alt!(parse_hex_int | parse_oct_int | parse_dec_int))
);

named!(pub parse_boolean<CompleteStr, bool>,
       alt!(
           tag!("true")  => { |_| true  } |
           tag!("false") => { |_| false }
       )
);

named!(pub parse<CompleteStr, ast::Node>,
       alt!(
           parse_boolean => { |b| ast::Node::Boolean(b)   } |
           parse_string  => { |s| ast::Node::TFString(s)  } |
           parse_integer => { |i| ast::Node::TFInteger(i) }
       )
);
