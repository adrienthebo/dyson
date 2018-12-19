//! An implementation of the Hashicorp Configuration Language.
//!
//! References:
//!     - [hashicorp/hcl](https://github.com/hashicorp/hcl/tree/v1.0.0)

#[macro_use]
extern crate nom;
extern crate serde_json;
extern crate unicode_xid;

pub mod ast;
pub mod parser;
pub mod issues;

#[macro_use]
pub mod macros;
