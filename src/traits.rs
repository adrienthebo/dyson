use ast;
use serde_json::map::Map as JsonMap;
use serde_json::Value as JsonValue;

impl From<ast::Node> for JsonValue {
    fn from(node: ast::Node) -> JsonValue {
        match node {
            ast::Node::TFInteger(i) => JsonValue::from(i),
            ast::Node::TFString(s) => JsonValue::from(s),
            ast::Node::Array(a) => JsonValue::from(a),
            ast::Node::Boolean(b) => JsonValue::from(b),
            ast::Node::TFFloat(f) => JsonValue::from(f),
            ast::Node::KeyValue(k, v) => {
                let mut m: JsonMap<String, JsonValue> = JsonMap::new();
                m.insert(k, JsonValue::from(v));

                m.into()
            }
            ast::Node::ObjectList(ol) => {
                let json_map = ol
                    .into_iter()
                    .fold(JsonMap::new(), |mut acc, (key, value)| {
                        acc.insert(key, JsonValue::from(value));
                        acc
                    });

                JsonValue::from(json_map)
            }
        }
    }
}
