use serde_json;
use std::error::Error;
use std::fmt;
use super::ast;

#[derive(Debug)]
pub struct AstEvalError;

impl fmt::Display for AstEvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AST evaluation error")
    }
}

impl Error for AstEvalError {
    fn description(&self) -> &str {
        "AST evaluation error"
    }
}


/// The result of evaluating an AST node. AST nodes may be noops, such as comments; this
/// is indicated by a return value of `Ok(None)`.
pub type AstResult = Result<Option<serde_json::Value>, AstEvalError>;

/// Evaluate an AST node to into JSON.
trait AstToJson {
    /// Convert this AST node to JSON.
    fn ast_to_json(&self) -> AstResult;
}

impl AstToJson for ast::Comment {
    /// Comments do not yield any JSON value.
    fn ast_to_json(&self) -> AstResult {
        Ok(None)
    }
}

impl AstToJson for ast::Identifier {
    fn ast_to_json(&self) -> AstResult {
        Ok(Some(serde_json::Value::String(self.0.clone())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let ident = ast::Identifier("hello".to_string());
        let json = ident.ast_to_json()
            .expect("AST eval error")
            .expect("Empty eval result");

        assert_eq!(
            json,
            serde_json::Value::String("hello".to_string())
        )
    }
}
