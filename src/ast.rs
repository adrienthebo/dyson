#[derive(Debug, PartialEq)]
pub enum Node {
    TFString(String),
    TFInteger(i64),
    Boolean(bool),
}
