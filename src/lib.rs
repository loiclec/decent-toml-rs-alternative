
mod tokens;
mod parser;
mod value;
mod printer;
mod from;
mod to;

use std::collections::HashMap;

pub use parser::parse_toml_lines;
pub use value::{TomlValue, TomlError, toml_value_from_lines, toml_value_from_parser_value};
pub use printer::print;
pub use from::FromToml;
pub use to::ToToml;

pub fn parse_toml(input: &str) -> Result<HashMap<String, TomlValue>, TomlError> {
    let lines = parse_toml_lines(input)?;
    let value = toml_value_from_lines(lines)?;
    Ok(value)
}

pub fn to_toml_file_content(value: TomlValue) -> String {
    if let TomlValue::Table(table) = value {
        print(&table)
    } else {
        let mut map = HashMap::new();
        map.insert("value".to_string(), value);
        print(&map)
    }
}