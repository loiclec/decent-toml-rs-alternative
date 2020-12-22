
use std::collections::HashMap;

use crate::TomlValue;

enum DottedKey<'a> {
    Key(&'a str, Box<DottedKey<'a>>),
    Empty
}
impl<'a> DottedKey<'a> {
    fn adding(&self, k: &'a str) -> Self {
        match self {
            DottedKey::Key(key, child) => {
                let new_child = Box::new(child.adding(k));
                DottedKey::Key(key, new_child)
            }
            DottedKey::Empty => {
                DottedKey::Key(k, Box::new(DottedKey::Empty))
            }
        }
    }
}

fn write_key(key: &str, to: &mut String) {
    if key.is_ascii() && key.chars().all(|c| matches!(c, '0' ..= '9' | 'a' ..= 'z' | 'A' ..= 'Z' | '-' | '_')) {
        to.push_str(&format!("{}", key));
    } else {
        to.push_str(&format!("\"{}\"", key.escape_default()));
    }
}
fn write_key_with_base(key: &str, base: &DottedKey, to: &mut String) {
    let mut base_iter = base;
    while let DottedKey::Key(k, next) = base_iter {
        write_key(k, to);
        to.push('.');
        base_iter = next;
    }
    write_key(key, to);
}

pub fn print(toml: &HashMap<String, TomlValue>) -> String {
    let mut to = String::new();
    print_rec(toml, &mut to, &DottedKey::Empty);
    to
}

fn print_rec(toml: &HashMap<String, TomlValue>, to: &mut String, parent_key: &DottedKey) {
    let mut table = vec![];
    let mut array_of_tables = vec![];
    let mut non_table_or_array = vec![];
    for (key, value) in toml {
        if let TomlValue::Array(x) = value {
            if x.iter().all(|e| matches!(e, TomlValue::Table(_))) {
                array_of_tables.push((key, x));
            } else {
                non_table_or_array.push((key, value));    
            }    
        } else if let TomlValue::Table(x) = value {
            table.push((key, x));
        } else {
            non_table_or_array.push((key, value));
        }
    }
    for (key, value) in non_table_or_array {
        write_key(key, to);
        to.push_str(" = ");
        value.print(to);
        to.push('\n');
    }
    for (key, table) in table {
        to.push('[');
        write_key_with_base(key, parent_key, to);
        to.push(']');
        to.push('\n');
        let new_parent = parent_key.adding(key);
        print_rec(table, to, &new_parent);
    }
    for (key, array) in array_of_tables {
        let new_parent = parent_key.adding(key);
        for x in array {
            if let TomlValue::Table(x) = x {
                to.push_str("[[");
                write_key_with_base(key, parent_key, to);
                to.push_str("]]\n");
                print_rec(x, to, &new_parent);
            } else {
                unreachable!();
            }
        } 
    }
}

impl TomlValue {
    pub fn print(&self, string: &mut String) {
        match self {
            TomlValue::String(x) => {
                string.push_str(&format!("\"{}\"", x.escape_default()));
            }
            TomlValue::Integer(x) => {
                string.push_str(&format!("{}", x));
            }
            TomlValue::Float(x) => {
                string.push_str(&format!("{}", x));
            }
            TomlValue::Boolean(x) => {
                string.push_str(&format!("{}", x));
            }
            TomlValue::Datetime(x) => {
                string.push_str(&format!("{}", x));
            }
            TomlValue::Array(x) => {
                string.push('[');
                for y in x {
                    y.print(string);
                    string.push_str(", ");
                }
                string.push(']');
            }
            TomlValue::Table(x) => {
                string.push('{');
                for (key, value) in x {
                    write_key(key, string);
                    string.push_str(" = ");
                    value.print(string);
                    string.push_str(", ");
                }
                string.push('}');
            }
        }
    }
}