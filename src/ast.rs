use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::str::FromStr;
use std::slice::SliceConcatExt;

pub type ObjectList = HashMap<String, Node>;

#[derive(Debug, PartialEq)]
pub enum Node {
    TFString(String),
    TFInteger(i64),
    TFFloat(f32),
    Boolean(bool),
    Array(Vec<Node>),
    KeyValue(String, String),
    ObjectList(ObjectList),
    //Comment(Comment),
}

#[derive(Debug)]
pub struct ParseAstError;

impl fmt::Display for ParseAstError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unable to parse HCL AST node")
    }
}

impl Error for ParseAstError {
    fn description(&self) -> &str {
        "Unable to parse HCL comment"
    }
}

/// [HCL2 Comment][comment-spec]
///
/// [comment-spec]: https://github.com/hashicorp/hcl2/blob/master/hcl/hclsyntax/spec.md#comments-and-whitespace
///
///
#[derive(Debug, PartialEq, Clone)]
pub struct Comment(pub String);

impl FromStr for Comment {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<Comment, Self::Err> {
        Ok(Comment(s.into()))
    }
}

impl<'a> From<&'a str> for Comment {
    fn from(s: &str) -> Comment {
        Comment(s.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub String);

impl FromStr for Identifier {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<Identifier, Self::Err> {
        Ok(Identifier(s.into()))
    }
}

impl<'a> From<&'a str> for Identifier {
    fn from(s: &str) -> Identifier {
        Identifier(s.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableExpr(pub String);

impl FromStr for VariableExpr {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<VariableExpr, Self::Err> {
        Ok(VariableExpr(s.into()))
    }
}

impl<'a> From<&'a str> for VariableExpr {
    fn from(s: &str) -> VariableExpr {
        VariableExpr(s.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLit(pub String);

impl FromStr for StringLit {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<StringLit, Self::Err> {
        Ok(StringLit(s.into()))
    }
}

impl<'a> From<&'a str> for StringLit {
    fn from(s: &str) -> StringLit {
        StringLit(s.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumericLit(pub f64);

impl FromStr for NumericLit {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<NumericLit, Self::Err> {
        Ok(NumericLit(f64::from_str(s).unwrap()))
    }
}

impl From<f64> for NumericLit {
    fn from(f: f64) -> NumericLit {
        NumericLit(f)
    }
}

/// [HCL2 configuration file][configuration-file]
///
/// [configuration-file]: https://github.com/hashicorp/hcl2/blob/master/hcl/hclsyntax/spec.md#configuration-files#[derive(Debug, PartialEq, Clone)]
#[derive(Debug, PartialEq, Clone)]
pub struct ConfigFile(pub Body);

pub type BodyItems = Vec<BodyItem>;

/// [HCL2 body][body]
///
/// A body is a collection of associated attributes and blocks. The meaning of this association is
/// defined by the calling application.
///
/// [body]: https://github.com/hashicorp/hcl2/blob/master/hcl/hclsyntax/spec.md#bodies
#[derive(Debug, PartialEq, Clone)]
pub struct Body(pub BodyItems);

#[derive(Debug, PartialEq, Clone)]
pub enum BodyItem {
    AttrItem(Attribute),
    BlockItem(Block),
}

impl From<Attribute> for BodyItem {
    fn from(attr: Attribute) -> BodyItem {
        BodyItem::AttrItem(attr)
    }
}

impl From<Block> for BodyItem {
    fn from(block: Block) -> BodyItem {
        BodyItem::BlockItem(block)
    }
}

/// [Attribute][attribute]
///
/// # Definition
///
/// An attribute definition assigns a value to a particular attribute name within a body. Each
/// distinct attribute name may be defined no more than once within a single body.
///
/// The attribute value is given as an expression, which is retained literally for later evaluation
/// by the calling application.
///
/// # Grammar
///
/// `Attribute = Identifier "=" Expression Newline`
///
/// [attribute]: https://github.com/hashicorp/hcl2/blob/master/hcl/hclsyntax/spec.md#attribute-definitions
#[derive(Debug, PartialEq, Clone)]
pub struct Attribute {
    pub ident: Identifier,
    pub expr: Expression,
}

/// Block label
///
/// # Definition
///
/// Block labels can either be quoted literal strings or naked identifiers.
///
/// # Grammar
///
/// `BlockLabel = StringLit | Identifier`
///
#[derive(Debug, PartialEq, Clone)]
pub enum BlockLabel {
    Identifier(Identifier),
    StringLit(StringLit)
}

/// Block Labels
///
/// # Definition
///
/// Block labels are one or more identifiers attached to a block.
///
/// # Grammar
///
/// `BlockLabels = (StringLit|Identifier)* "{" Newline Body "}" Newline;
///
/// # Notes
///
/// The Blocklabels production does not exist in the official HCL2 grammar
pub type BlockLabels = Vec<BlockLabel>;

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub ident: Identifier,
    pub labels: BlockLabels,
    pub body: Body,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    ExprTerm(ExprTerm),
    Operation,
    Conditional,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprTerm {
    LiteralValue(LiteralValue),
    CollectionValue(CollectionValue),
    TemplateExpr(TemplateExpr),
    VariableExpr(VariableExpr),
    FunctionCall(FunctionCall),

    // ForExprs contain expressions so must be indirected to prevent overly
    // recursive type definitions.
    ForExpr(ForExpr),

    //ExprTerm Index,
    //ExprTerm GetAttr,
    //ExprTerm Splat,
}

impl From<bool> for ExprTerm {
    fn from(b: bool) -> ExprTerm {
        ExprTerm::LiteralValue(b.into())
    }
}

impl From<f64> for ExprTerm {
    fn from(f: f64) -> ExprTerm {
        ExprTerm::LiteralValue(f.into())
    }
}

impl From<CollectionValue> for ExprTerm {
    fn from(c: CollectionValue) -> ExprTerm {
        ExprTerm::CollectionValue(c)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    NumericLit(NumericLit),
    True,
    False,
    Null,
}

impl From<LiteralValue> for ExprTerm {
    fn from(l: LiteralValue) -> ExprTerm {
        ExprTerm::LiteralValue(l)
    }
}

impl From<NumericLit> for LiteralValue {
    fn from(n: NumericLit) -> LiteralValue {
        LiteralValue::NumericLit(n)
    }
}

impl From<bool> for LiteralValue {
    fn from(b: bool) -> LiteralValue {
        match b {
            true => LiteralValue::True,
            false => LiteralValue::False,
        }
    }
}

impl From<f64> for LiteralValue {
    fn from(f: f64) -> LiteralValue {
        LiteralValue::NumericLit(f.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CollectionValue {
    Tuple(Tuple),
    Object(Object),
}

pub type Tuple = Vec<Expression>;

pub type Object = Vec<ObjectElem>;

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectElem {
    pub key: ObjectKey,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectKey {
    Identifier(Identifier),
    Expression(Expression),
}

/// Template Regions
///
/// # Definition
///
/// A template region is a contiguous set of bytes, composing either a string
/// literal or an interpolated expression.
///
/// # Grammar
///
/// `TemplateRegion = (StringLit | "${" Expression "}")*`
///
/// # Notes
///
/// The TemplateRegion production does not exist in the official HCL2 grammar
#[derive(Debug, PartialEq, Clone)]
pub enum TemplateRegion {
    StringLit(StringLit),
    Expression(Expression)
}

impl From<StringLit> for TemplateRegion {
    fn from(s: StringLit) -> TemplateRegion {
        TemplateRegion::StringLit(s)
    }
}

impl From<Expression> for TemplateRegion {
    fn from(e: Expression) -> TemplateRegion {
        TemplateRegion::Expression(e)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TemplateExpr(pub String);

impl<'a> From<&'a str> for TemplateExpr {
    fn from(s: &'a str) -> TemplateExpr {
        TemplateExpr(s.to_string())
    }
}

impl TemplateExpr {
    pub fn from_heredoc<'a>(s: &'a str, do_trim: bool) -> TemplateExpr {
        match do_trim {
            true => {
                let lines = s.split('\n');
                let to_trim = lines
                    // Select all lines with at least one leading whitespace
                    .filter(|l| l.starts_with(' '))
                    // And then extract the longest whitespace substring
                    .map(|l| l.chars().take_while(|ch| ch == &' ').collect::<Vec<char>>())
                    // And then return the shortest whitespace count
                    .fold(s.len(), |acc, prefix| {
                        println!("acc = {}, prefix = {:?}", acc, prefix);
                        ::std::cmp::min(acc, prefix.len())
                    });

                println!("totrim = {}", to_trim);
                let trimmed = s.split('\n')
                    .map(|line| {
                        if line.starts_with(' ') {
                            line[to_trim..].to_string()
                        } else {
                            line.to_string()
                        }
                    }).collect::<Vec<String>>().join("\n");

                TemplateExpr(trimmed)
            },
            false => TemplateExpr(s.to_string())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub ident: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForCond(pub Box<Expression>);

#[derive(Debug, PartialEq, Clone)]
pub struct ForIntro {
    pub idents: (Identifier, Option<Identifier>),
    pub expr: Box<Expression>
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForTupleExpr {
    pub intro: ForIntro,
    pub expr: Box<Expression>,
    pub cond: Option<ForCond>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForObjectExpr {
    pub intro: ForIntro,
    pub k_expr: Box<Expression>,
    pub v_expr: Box<Expression>,
    pub group: bool,
    pub cond: Option<ForCond>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForExpr {
    ForTupleExpr(ForTupleExpr),
    ForObjectExpr(ForObjectExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexOp(pub Box<Expression>);

#[derive(Debug, PartialEq, Clone)]
pub enum SplatOp {
    AttrSplat(Vec<GetAttr>),
    FullSplat(Vec<FullSplat>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FullSplat {
    GetAttr(GetAttr),
    IndexOp(IndexOp),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprTermAccess {
    IndexOp(IndexOp),
    GetAttr(GetAttr),
    SplatOp(SplatOp),
}

pub type GetAttr = Identifier;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_templateexpr_trim_1() {
        let text = "This string is\n  prefixed with\n  two spaces";
        let tmpl = TemplateExpr::from_heredoc(text, true);
        assert_eq!(tmpl.0, "This string is\nprefixed with\ntwo spaces");
    }

    #[test]
    fn test_templateexpr_trim_2() {
        let text = "    This string is\n  prefixed with\n  two spaces";
        let tmpl = TemplateExpr::from_heredoc(text, true);
        assert_eq!(tmpl.0, "  This string is\nprefixed with\ntwo spaces");
    }

    #[test]
    fn test_templateexpr_trim_3() {
        let text = "This string is\n prefixed with\n  one space";
        let tmpl = TemplateExpr::from_heredoc(text, true);
        assert_eq!(tmpl.0, "This string is\nprefixed with\n one space");
    }
}
