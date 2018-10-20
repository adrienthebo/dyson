use super::ast;
use nom::types::CompleteStr;
use std::collections::HashMap;
use std::str::FromStr;

pub type ParseResult<'a> = nom::IResult<CompleteStr<'a>, ast::Node>;

named!(pub kvp(CompleteStr) -> (String, ast::Node),
        ws!(do_parse!(
            key: ws!(string)    >>
            _eq: ws!(tag!("=")) >>
            value: ws!(parse)   >>
            ((key, value))
        ))
);

named!(pub string(CompleteStr) -> String,
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

named!(pub multi_line_comment(CompleteStr) -> String,
       preceded!(
           tag!("/*"),
            map_res!(
                take_until_and_consume!("*/"),
                |s: CompleteStr| { FromStr::from_str(s.0) }
            )
        )
);

named!(pub single_line_comment(CompleteStr) -> String,
       preceded!(
           alt!(tag!("//") | tag!("#")),
            map_res!(
                take_until_and_consume!("\n"),
                |s: CompleteStr| { FromStr::from_str(s.0) }
            )
        )
);

named!(pub hex_int(CompleteStr) -> i64,
        ws!(
           map_res!( preceded!(tag!("0x"), nom::hex_digit1), |s:  CompleteStr| {  i64::from_str_radix(s.0, 16) })
        )
);

named!(pub oct_int(CompleteStr) -> i64,
        ws!(
            map_res!( preceded!(tag!("0"), nom::oct_digit1), |s:  CompleteStr| {  i64::from_str_radix(s.0, 8) })
        )
);

named!(pub dec_int(CompleteStr) -> i64,
        ws!(map_res!(nom::digit1, |s:  CompleteStr| {  i64::from_str_radix(s.0, 10) }))
);

named!(pub integer(CompleteStr) -> i64,
       ws!(alt!(hex_int | oct_int | dec_int))
);

named!(pub unsigned_float(CompleteStr) -> f32,
        ws!(
           flat_map!(
               recognize!(delimited!(nom::digit, char!('.'), nom::digit)),
               parse_to!(f32)
            )
        )
);

named!(pub exp_float(CompleteStr) -> f32,
        ws!(
           flat_map!(
               recognize!(
                   delimited!(
                       alt!(
                           unsigned_float |
                           flat_map!(nom::digit, parse_to!(f32))
                       ),
                       char!('e'),
                       nom::digit
                   )
               ),
               parse_to!(f32)
            )
       )
);

named!(pub float(CompleteStr) -> f32,
       ws!(alt!(exp_float))
);

named!(pub boolean(CompleteStr) -> bool,
        ws!(
           alt!(
               tag!("true")  => { |_| true  } |
               tag!("false") => { |_| false }
           )
        )
);

named!(pub array(CompleteStr) -> Vec<ast::Node>,
    ws!(
       delimited!(
           char!('['),
           separated_list!(ws!(char!(',')), parse),
           char!(']')
        )
       )
);

named!(pub object(CompleteStr) -> HashMap<String, ast::Node>,
        fold_many1!(ws!(kvp), HashMap::new(), |mut acc: HashMap<String, ast::Node>, (key, value)| {
            println!("{} => {:?}", key, value);
            acc.insert(key, value);
            acc
        })
);

named!(pub parse(CompleteStr) -> ast::Node,
    dbg_dmp!(
        ws!(
            alt!(
                boolean => { |b| ast::Node::Boolean(b)   }  |
                string  => { |s| ast::Node::TFString(s)  }  |
                float   => { |f| ast::Node::TFFloat(f)   }  |
                integer => { |i| ast::Node::TFInteger(i) }  |
                array   => { |v| ast::Node::Array(v)     }  |
                object  => { |o| ast::Node::ObjectList(o) }
            )
        )
    )
);
