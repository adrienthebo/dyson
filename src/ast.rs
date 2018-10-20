use std::collections::HashMap;

pub type ObjectList = HashMap<String, Node>;

#[derive(Debug, PartialEq)]
pub enum Node {
    TFString(String),
    TFInteger(i64),
    TFFloat(f32),
    Boolean(bool),
    Array(Vec<Node>),
    KeyValue(String, String),
    ObjectList(ObjectList),
}
