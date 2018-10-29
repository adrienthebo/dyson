//! HCL parsers.

pub mod v2;

use ast;
use nom;

pub type ParseResult<'a> = nom::IResult<nom::types::CompleteStr<'a>, ast::Node>;
