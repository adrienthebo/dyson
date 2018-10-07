//! An implementation of the Hashicorp Configuration Language.
//!
//! References:
//!     - [hashicorp/hcl](https://github.com/hashicorp/hcl)

#[macro_use]
extern crate nom;

pub mod ast;
pub mod parser;
