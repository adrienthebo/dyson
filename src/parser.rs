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

named!(pub parse_boolean<CompleteStr, bool>,
       alt!(
           tag!("true")  => { |_| true  } |
           tag!("false") => { |_| false }
       )
);

named!(pub parse<CompleteStr, ast::Node>,
       alt!(
           parse_boolean => { |b| ast::Node::Boolean(b) } |
           parse_string  => { |s| ast::Node::TFString(s) }
       )
);
