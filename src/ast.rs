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

#[derive(Debug, PartialEq, Clone)]
pub struct Comment(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub String);

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

impl FromStr for Comment {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<Comment, Self::Err> {
        Ok(Comment(s.into()))
    }
}

impl FromStr for Identifier {
    type Err = ParseAstError;

    fn from_str(s: &str) -> Result<Identifier, Self::Err> {
        println!("HO HO HO I'M CONVERTING AN IDENTIFIER");
        Ok(Identifier(s.into()))
    }
}
