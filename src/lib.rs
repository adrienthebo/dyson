//! An implementation of the Hashicorp Configuration Language.
//!
//! References:
//!     - [hashicorp/hcl](https://github.com/hashicorp/hcl/tree/v1.0.0)
#![feature(rustc_private)]

#[macro_use]
extern crate nom;
extern crate serde_json;

pub mod ast;
pub mod parser;
pub mod issues;

#[macro_use]
pub mod macros;
