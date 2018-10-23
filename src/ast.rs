use std::collections::HashMap;
use std::str::FromStr;
use std::error::Error;
use std::fmt;

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
pub struct NumericLit(pub f64);

impl FromStr for NumericLit {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<NumericLit, Self::Err> {
        Ok(NumericLit(f64::from_str(s).unwrap()))
    }
}
