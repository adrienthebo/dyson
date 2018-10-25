use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::str::FromStr;

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

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub String);

impl FromStr for Identifier {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<Identifier, Self::Err> {
        Ok(Identifier(s.into()))
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

#[derive(Debug, PartialEq, Clone)]
pub struct StringLit(pub String);

impl FromStr for StringLit {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<StringLit, Self::Err> {
        Ok(StringLit(s.into()))
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

/// [HCL2 configuration file][configuration-file]
///
/// [configuration-file]: https://github.com/hashicorp/hcl2/blob/master/hcl/hclsyntax/spec.md#configuration-files#[derive(Debug, PartialEq, Clone)]
#[derive(Debug, PartialEq, Clone)]
pub struct ConfigFile(pub Body);

pub type BodyItems = Vec<BodyItem>;

/// [HCL2 body][body]
///
/// [body]: https://github.com/hashicorp/hcl2/blob/master/hcl/hclsyntax/spec.md#bodies
#[derive(Debug, PartialEq, Clone)]
pub struct Body(pub BodyItems);

#[derive(Debug, PartialEq, Clone)]
pub enum BodyItem {
    AttrItem(Attribute),
    BlockItem(Block),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Attribute {
    pub ident: Identifier,
    pub expr: Expression,
}

pub type BlockLabels = Vec<Identifier>;

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
    //ForExpr,
    //ExprTerm Index,
    //ExprTerm GetAttr,
    //ExprTerm Splat,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    NumericLit(NumericLit),
    True,
    False,
    Null,
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

#[derive(Debug, PartialEq, Clone)]
pub struct TemplateExpr(pub String);

impl FromStr for TemplateExpr {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<TemplateExpr, Self::Err> {
        Ok(TemplateExpr(s.to_string()))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub ident: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForCond(pub Expression);

#[derive(Debug, PartialEq, Clone)]
pub struct ForIntro {
    pub idents: (Identifier, Option<Identifier>),
    pub expr: Expression
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForTupleExpr {
    pub intro: ForIntro,
    pub expr: Expression,
    pub cond: Option<ForCond>,
}
