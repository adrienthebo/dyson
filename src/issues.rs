//! HCL errors
//!

/*
pub enum ErrorKind {
    SyntaxError(u32, String),
}

pub struct Error {
    id: u32,


pub 'static 
*/

// TODO: look at how Henrik implemented issues in Puppet

pub enum ErrorCode {
    E00001 = 1,
    E00002 = 2,
    E00003 = 3,
    E00004 = 4
}

impl std::string::ToString for ErrorCode {
    fn to_string(&self) -> String {
        match self {
            ErrorCode::E00001 => "Expected LBRACE (eg '{')".to_string(),
            ErrorCode::E00002 => "Expected RBRACE (eg '}')".to_string(),
            ErrorCode::E00003 => "Expected Newline".to_string(),
            ErrorCode::E00004 => "Unexpected token".to_string(),
        }
    }
}

