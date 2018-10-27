use ast::*;
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
                Ok((ltrim, o)) => {
                    match (hsp)(ltrim) {
                        Err(e) => Err(Err::convert(e)),
                        Ok((trim, _)) => Ok((trim, o))
                    }
                }
            }
        }
    )
);

named!(pub multi_line_comment(CompleteStr) -> Comment,
    flat_map!(
        delimited!(
            tag!("/*"),
            take_until!("*/"),
            tag!("*/")
        ),
        parse_to!(Comment)
    )
);

named!(pub single_line_comment(CompleteStr) -> Comment,
    delimited!(
        alt!(tag!("#") | tag!("//")),
        map!(
            take_till!(|ch| ch == '\r' || ch == '\n'),
            |s| { Comment(s.to_string()) }
        ),
        opt!(call!(nom::line_ending))
    )
);

named!(pub comment(CompleteStr) -> Comment,
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
    input.split_at_position(|item| {
        let ch = item.as_char();
        !(ch.is_xid_continue() || ch == '-')
    })
}

named!(pub identifier(CompleteStr) -> Identifier,
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
        parse_to!(Identifier)
    )
);

named!(pub variable_expr(CompleteStr) -> VariableExpr,
    map!(
        identifier,
        |ident| { VariableExpr(ident.0) }
    )
);

named!(pub numericlit(CompleteStr) -> NumericLit,
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
        parse_to!(NumericLit)
    )
);

named!(pub attribute(CompleteStr) -> Attribute,
    map!(
        separated_pair!(
            identifier,
            char!('='),
            expression
        ),
        |(ident, expr)| { Attribute { ident, expr } }
    )
);

named!(pub blocklabels(CompleteStr) -> BlockLabels,
    many0!(identifier)
);

named!(pub block(CompleteStr) -> Block,
    do_parse!(
        ident: identifier                   >>
        labels: blocklabels                 >>
        pair!(char!('{'), nom::line_ending) >>
        inner: body                         >>
        pair!(char!('}'), nom::line_ending) >>
        (Block { ident, labels, body: inner })
    )
);

named!(pub bodyitem(CompleteStr) -> BodyItem,
    alt!(
        attribute => { |a| BodyItem::AttrItem(a) } |
        block     => { |b| BodyItem::BlockItem(b) }
    )
);

named!(pub body(CompleteStr) -> Body,
    map!(
        many0!(bodyitem),
        |v: BodyItems| { Body(v) }
    )
);

named!(pub literalvalue(CompleteStr) -> LiteralValue,
    alt!(
        numericlit    => { |nl| LiteralValue::NumericLit(nl) } |
        tag!("true")  => { |_| LiteralValue::True } |
        tag!("false") => { |_| LiteralValue::False } |
        tag!("null")  => { |_| LiteralValue::Null }
    )
);

named!(pub tuple(CompleteStr) -> Tuple,
    delimited!(
        char!('['),
        separated_list!(char!(','), expression),
        char!(']')
    )
);

named!(pub objectkey(CompleteStr) -> ObjectKey,
    alt!(
        identifier => { |i| ObjectKey::Identifier(i) } |
        expression => { |e| ObjectKey::Expression(e) }
    )
);

named!(pub objectelem(CompleteStr) -> ObjectElem,
    map!(
        hws!(
            separated_pair!(
                objectkey,
                char!('='),
                expression
            )
        ),
        |(key, value)| { ObjectElem { key, value }}
    )
);

named!(pub objectelem_term(CompleteStr) -> CompleteStr,
    alt!(
        recognize!(pair!(tag!(","), nom::line_ending)) |
        nom::line_ending                               |
        tag!(",")
    )
);

named!(pub object(CompleteStr) -> Object,
    hws!(
        do_parse!(
            char!('{')                                         >>
            opt!(nom::line_ending)                             >>
            list: separated_list!(objectelem_term, objectelem) >>
            opt!(nom::line_ending)                             >>
            char!('}')                                         >>
            (list)
        )
    )
);

named!(pub expression(CompleteStr) -> Expression,
    hws!(
        alt!(
            exprterm => { |et| Expression::ExprTerm(et) }
        )
    )
);

named!(pub exprterm(CompleteStr) -> ExprTerm,
    alt!(
        literalvalue    => { |lv| ExprTerm::LiteralValue(lv) } |
        collectionvalue => { |cv| ExprTerm::CollectionValue(cv) } |
        template_expr   => { |t| ExprTerm::TemplateExpr(t) } |
        functioncall    => { |f| ExprTerm::FunctionCall(f) } |
        variable_expr   => { |v| ExprTerm::VariableExpr(v) }
    )
);

named!(pub collectionvalue(CompleteStr) -> CollectionValue,
    hws!(
        alt!(
            tuple  => { |t| CollectionValue::Tuple(t) } |
            object => { |o| CollectionValue::Object(o) }
        )
    )
);

named!(pub quoted_template(CompleteStr) -> TemplateExpr,
    hws!(
        flat_map!(
            delimited!(
                char!('"'),
                take_until!(r#"""#),
                char!('"')
            ),
            parse_to!(TemplateExpr)
        )
    )
);

/// todo: implement template trimming
named!(pub heredoc_template(CompleteStr) -> TemplateExpr,
    hws!(
        do_parse!(
            tag!("<<")                   >>
            _do_trim: opt!(char!('-'))   >>
            ident: identifier            >>
            call!(nom::line_ending)      >>
            s: take_until!(&ident.0[..]) >>
            (TemplateExpr(s.to_string()))
        )
    )
);

named!(pub template_expr(CompleteStr) -> TemplateExpr,
    alt!(quoted_template | heredoc_template)
);

named!(pub stringlit(CompleteStr) -> StringLit,
    hws!(
        delimited!(
            char!('"'),
            map!(
                escaped_transform!(
                    is_not!("\\\"\n"),
                    '\\',
                    alt!(
                        tag!("\\") => { |_| &"\\"[..] } |
                        tag!("\"") => { |_| &"\""[..] } |
                        tag!("n")  => { |_| &"\n"[..] }
                    )
                ),
                |s: String| { StringLit(s) }
            ),
            char!('"')
        )
    )
);

named!(pub functioncall(CompleteStr) -> FunctionCall,
    map!(
        pair!(
            identifier,
            delimited!(
                char!('('),
                separated_list!(char!(','), expression),
                char!(')')
            )
        ),
        |(ident, arguments)| { FunctionCall { ident, arguments } }
    )
);

named!(pub for_cond(CompleteStr) -> ForCond,
    map!(
        preceded!(tag!("if"), expression),
        |e: Expression| { ForCond(Box::new(e)) }
    )
);

named!(pub for_intro(CompleteStr) -> ForIntro,
    do_parse!(
        tag!("for")                                                        >>
        idents: pair!(identifier, opt!(preceded!(char!(','), identifier))) >>
        tag!("in")                                                         >>
        expr: expression                                                   >>
        char!(':')                                                         >>
        (ForIntro { idents, expr: Box::new(expr) })
    )
);

named!(pub for_tuple_expr(CompleteStr) -> ForTupleExpr,
    do_parse!(
        char!('[')           >>
        intro: for_intro     >>
        expr: expression     >>
        cond: opt!(for_cond) >>
        char!(']')           >>
        (ForTupleExpr { intro, expr: Box::new(expr), cond })
    )
);

named!(pub for_object_expr(CompleteStr) -> ForObjectExpr,
    do_parse!(
        char!('{')               >>
        intro: for_intro         >>
        k_expr: expression       >>
        tag!("=>")               >>
        v_expr: expression       >>
        group: opt!(tag!("...")) >>
        cond: opt!(for_cond)     >>
        char!('}')               >>
        (ForObjectExpr { intro, k_expr: Box::new(k_expr), v_expr: Box::new(v_expr), group: group.is_some(), cond })
    )
);

named!(pub for_expr(CompleteStr) -> ForExpr,
    alt!(
        for_tuple_expr  => { |t| ForExpr::ForTupleExpr(t) } |
        for_object_expr => { |o| ForExpr::ForObjectExpr(o) }
    )
);

named!(pub index_op(CompleteStr) -> IndexOp,
    map!(
        delimited!(
            char!('['),
            expression,
            char!(']')
        ),
        |e| { IndexOp(Box::new(e)) }
    )
);

named!(pub get_attr(CompleteStr) -> GetAttr,
    preceded!(
        tag!("."),
        identifier
    )
);

named!(pub attr_splat(CompleteStr) -> SplatOp,
    map!(
        preceded!(
            tag!(".*"),
            many0!(get_attr)
        ),
        |a| {  SplatOp::AttrSplat(a) }
    )
);

named!(pub full_splat(CompleteStr) -> SplatOp,
    map!(
        preceded!(
            tag!("[*]"),
            many0!(
                alt!(
                    get_attr => { |a| FullSplat::GetAttr(a) } |
                    index_op => { |i| FullSplat::IndexOp(i) }
                )
            )
        ),
        |a| {  SplatOp::FullSplat(a) }
    )
);

named!(pub splat_op(CompleteStr) -> SplatOp,
    alt!(attr_splat | full_splat)
);

named!(pub expr_term_access(CompleteStr) -> ExprTermAccess,
    alt!(
        index_op => { |i| ExprTermAccess::IndexOp(i) } |
        get_attr => { |a| ExprTermAccess::GetAttr(a) } |
        splat_op => { |s| ExprTermAccess::SplatOp(s) }
    )
);
