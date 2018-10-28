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
    E00001 = 1
}

impl std::string::ToString for ErrorCode {
    fn to_string(&self) -> String {
        match self {
            E00001 => "Expected LBRACE (eg '{')".to_string(),
        }
    }
}

