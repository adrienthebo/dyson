//! A trivial HCL evaluator.
//!
//! At the moment evaluating means "convert this to JSON so that we can write tests for the
//! parser."

use super::ast;
use std::error::Error;
use std::fmt;

use serde_json::Value as JsonValue;

#[derive(Debug)]
pub struct EvalError;

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "eval error")
    }
}

impl Error for EvalError {
    fn description(&self) -> &str {
        "Evaluation error"
    }
}

pub fn eval(node: ast::Node) -> Result<JsonValue, EvalError> {
    Ok(JsonValue::from(node))
}
