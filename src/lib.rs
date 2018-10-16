//! An implementation of the Hashicorp Configuration Language.
//!
//! References:
//!     - [hashicorp/hcl](https://github.com/hashicorp/hcl/tree/v1.0.0)

#[macro_use]
extern crate nom;
extern crate serde_json;

pub mod ast;
pub mod evaluator;
pub mod parser;
pub mod traits;
