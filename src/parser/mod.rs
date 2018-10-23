//! HCL parsers.

pub mod v1;
pub mod v2;
pub use self::v1::*;

use nom;
use ast;

pub type ParseResult<'a> = nom::IResult<nom::types::CompleteStr<'a>, ast::Node>;
