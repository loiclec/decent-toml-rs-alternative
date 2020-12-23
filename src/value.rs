use core::panic;
use std::collections::HashMap;

use crate::{parser, tokens};

#[derive(Debug)]
pub enum TomlError {
    Deserializer(parser::Error),
    Tokens(tokens::Error),
    KeyRedefined(Vec<String>),
    ChangingInlineTable(Vec<String>),
    ValueTypeRedefined,
}
impl From<parser::Error> for TomlError {
    fn from(e: parser::Error) -> Self {
        Self::Deserializer(e)
    }
}
impl From<tokens::Error> for TomlError {
    fn from(e: tokens::Error) -> Self {
        Self::Tokens(e)
    }
}

// A full TOML file is a HashMap<String, TomlValue> 
/// Representation of a TOML value.
#[derive(PartialEq, Clone, Debug)]
pub enum TomlValue {
    /// Represents a TOML string
    String(String),
    /// Represents a TOML integer
    Integer(i64),
    /// Represents a TOML float
    Float(f64),
    /// Represents a TOML boolean
    Boolean(bool),
    /// Represents a TOML datetime
    Datetime(String),
    /// Represents a TOML array
    Array(Array),
    /// Represents a TOML table
    Table(Table),
}

/// Type representing a TOML array, payload of the `Value::Array` variant
pub type Array = Vec<TomlValue>;

/// Type representing a TOML table, payload of the `Value::Table` variant.
pub type Table = HashMap<String, TomlValue>;

fn append_table(table: &mut HashMap<String, TomlValue>, key_index: &[String]) -> Result<(), TomlError> {
    match &key_index {
        [] => {
            panic!("Trying to add a value to an empty key")
        },
        [key] => {
            if let Some(array) = table.get_mut(key) {
                match array {
                    TomlValue::Array(array) => {
                        array.push(TomlValue::Table(HashMap::new()));
                    }
                    _ => {
                        unreachable!("Should have been caught")
                    }
                }
            } else {
                table.insert(key.to_string(), TomlValue::Array(vec![TomlValue::Table(HashMap::new())]));
            }
            Ok(())
        },
        [key, rest @ ..] => {
            let subtable = table.entry(key.to_string()).or_insert_with(|| {
                TomlValue::Table(HashMap::new())
            });
            match subtable {
                // if it's an array, that's fine, take the last element
                TomlValue::Table(subtable) => {
                    append_table(subtable, rest)
                }
                TomlValue::Array(array) => {
                    if let Some(last) = array.last_mut() {
                        match last {
                            TomlValue::Table(subtable) => {
                                append_table(subtable, rest)
                            },
                            _ => {
                                Err(TomlError::ValueTypeRedefined)
                            }
                        }
                    } else {
                        unreachable!("Adding an element to the last table of an array of tables that is empty")
                    }
                }
                _ => {
                    Err(TomlError::ValueTypeRedefined)
                }
            }
        }
    }
}

fn create_table(table: &mut HashMap<String, TomlValue>, key_index: &[String]) -> Result<(), TomlError> {
    match &key_index {
        [] => {
            panic!("Trying to add a value to an empty key")
        },
        [key] => {
            table.entry(key.to_string()).or_insert_with(|| TomlValue::Table(HashMap::new()));
            Ok(())
        },
        [key, rest @ ..] => {
            let subtable = table.entry(key.to_string()).or_insert_with(|| {
                TomlValue::Table(HashMap::new())
            });
            match subtable {
                // if it's an array, that's fine, take the last element
                TomlValue::Table(subtable) => {
                    create_table(subtable, rest)
                }
                TomlValue::Array(array) => {
                    if let Some(last) = array.last_mut() {
                        match last {
                            TomlValue::Table(subtable) => {
                                create_table(subtable, rest)
                            },
                            _ => {
                                Err(TomlError::ValueTypeRedefined)
                            }
                        }
                    } else {
                        unreachable!("Adding an element to the last table of an array of tables that is empty")
                    }
                }
                _ => {
                    Err(TomlError::ValueTypeRedefined)
                }
            }
        }
    }
}

fn add_to_table(table: &mut HashMap<String, TomlValue>, key_index: &[String], value: TomlValue) -> Result<(), TomlError> {
    match &key_index {
        [] => {
            unreachable!("Trying to add a value to an empty key")
        },
        [key] => {
            if let Some(_) = table.insert(key.to_string(), value) {
                Err(TomlError::ValueTypeRedefined)
            } else {
                Ok(())
            }
        },
        [key, rest @ ..] => {
            let subtable = table.entry(key.to_string()).or_insert_with(|| {
                TomlValue::Table(HashMap::new())
            });
            match subtable {
                // if it's an array, that's fine, take the last element
                TomlValue::Table(subtable) => {
                    add_to_table(subtable, rest, value)
                }
                TomlValue::Array(array) => {
                    if let Some(last) = array.last_mut() {
                        match last {
                            TomlValue::Table(subtable) => {
                                add_to_table(subtable, rest, value)
                            },
                            _ => {
                                Err(TomlError::ValueTypeRedefined)
                            }
                        }
                    } else {
                        unreachable!("Adding an element to the last table of an array of tables that is empty")
                    }
                }
                _ => {
                    Err(TomlError::ValueTypeRedefined)
                }
            }
        }
    }
}

fn key_is_already_created_in_table(table: &HashMap<String, TomlValue>, key_index: &[String]) -> bool {
    match key_index {
        [] => { true }
        [key] => {
            table.contains_key(key)
        }
        [key, rest @ ..] => {
            if let Some(TomlValue::Table(subtable)) = table.get(key) {
                key_is_already_created_in_table(subtable, rest)
            } else {
                false
            }
        }
    }
}

pub fn toml_value_from_parser_value(value: parser::Value) -> TomlValue {
    match value.e {
        parser::ValueKind::Integer(x) => {
            TomlValue::Integer(x)
        }
        parser::ValueKind::Float(x) => {
            TomlValue::Float(x)
        }
        parser::ValueKind::Boolean(x) => {
            TomlValue::Boolean(x)
        }
        parser::ValueKind::String(x) => {
            TomlValue::String(x.into_owned())
        }
        parser::ValueKind::Datetime(x) => {
            TomlValue::Datetime(x.to_string())
        }
        parser::ValueKind::Array(x) => {
            TomlValue::Array(x.into_iter().map(|x| toml_value_from_parser_value(x)).collect())
        }
        parser::ValueKind::InlineTable(x) | parser::ValueKind::DottedTable(x) => {
            let mut map = HashMap::new();
            for ((_, key), value) in x.into_iter() {
                map.insert(key.into_owned(), toml_value_from_parser_value(value));
            }
            TomlValue::Table(map)
        }
    }
}

#[derive(Default)]
struct DefinedKeys {
    defined: bool,
    children: HashMap<String, DefinedKeys>
}
impl DefinedKeys {
    fn add_key_index(&mut self, key_index: &[String]) {
        match key_index {
            [] => {
                self.defined = true;
            }
            [key] => {
                let child = self.children.entry(key.to_string()).or_insert_with(|| DefinedKeys::default() );
                child.defined = true;
            }
            [key, rest @ ..] => {
                let child = self.children.entry(key.to_string()).or_insert_with(|| DefinedKeys::default() );
                child.add_key_index(rest);
            }
        }
    }
    fn is_key_index_defined(&self, key_index: &[String]) -> bool {
        match key_index {
            [] => {
                self.defined
            }
            [key] => {
                if let Some(child) = self.children.get(key) {
                    child.defined
                } else {
                    false
                }
            }
            [key, rest @ ..] => {
                if let Some(child) = self.children.get(key) {
                    child.is_key_index_defined(rest)
                } else {
                    false
                }
            }
        }
    }
    fn is_key_or_subkey_defined(&self, key_index: &[String]) -> bool {
        self.defined || match key_index {
            [] => {
                self.defined
            }
            [key] => {
                if let Some(child) = self.children.get(key) {
                    child.defined
                } else {
                    false
                }
            }
            [key, rest @ ..] => {
                if let Some(child) = self.children.get(key) {
                    child.is_key_or_subkey_defined(rest)
                } else {
                    false
                }
            }
        }
    }
    fn forget_subdefinitions_of_key(&mut self, key_index: &[String]) {
        match key_index {
            [] => {
                self.children.clear();
            }
            [key] => {
                if let Some(child) = self.children.get_mut(key) {
                    child.forget_subdefinitions_of_key(&[]);
                } else {
                    
                }
            }
            [key, rest @ ..] => {
                if let Some(child) = self.children.get_mut(key) {
                    child.forget_subdefinitions_of_key(rest);
                } else {
                    
                }
            }
        }
    }
}

pub fn toml_value_from_lines(lines: Vec<parser::Line<'_>>) -> Result<HashMap<String, TomlValue>, TomlError> {
    let mut value = HashMap::<String, TomlValue>::new();

    let mut key_indexes_to_define_at_end_of_table: Vec<Vec<String>> = vec![];
    let mut parent_key_index: Vec<String> = vec![];

    let mut immutable_key_indexes = DefinedKeys::default();
    let mut defined_key_indexes = DefinedKeys::default();

    for line in lines {
        match line {
            parser::Line::Table { at: _, mut header, array } => {
                for key_index in key_indexes_to_define_at_end_of_table.drain(..) {
                    defined_key_indexes.add_key_index(&key_index);
                }
                let mut key_index = vec![];
                while let Some((_, part)) = header.next()? {
                    key_index.push(part.into_owned());
                }
                if immutable_key_indexes.is_key_or_subkey_defined(&key_index) {
                    return Err(TomlError::ChangingInlineTable(key_index.clone()));
                }
                if array {
                    parent_key_index = key_index;
                    immutable_key_indexes.forget_subdefinitions_of_key(&parent_key_index);
                    defined_key_indexes.forget_subdefinitions_of_key(&parent_key_index);
                    defined_key_indexes.add_key_index(&parent_key_index);
                    append_table(&mut value, &parent_key_index)?;
                } else {
                    if defined_key_indexes.is_key_index_defined(&key_index) {
                        return Err(TomlError::KeyRedefined(key_index.clone()))
                    } else {
                        parent_key_index = key_index;
                        defined_key_indexes.add_key_index(&parent_key_index);
                        create_table(&mut value, &parent_key_index)?;
                    }
                }
            
            }
            parser::Line::KeyValue(key, new_value) => {
                let mut key_index = parent_key_index.clone();
                for (_, part) in key.into_iter() {
                    key_index.push(part.into_owned());
                    key_indexes_to_define_at_end_of_table.push(key_index.clone());
                    
                    if defined_key_indexes.is_key_index_defined(&key_index) {
                        return Err(TomlError::KeyRedefined(key_index.clone()))
                    }
                }
                if key_is_already_created_in_table(&value, &key_index) {
                    return Err(TomlError::KeyRedefined(key_index.clone()))
                } else {
                    let top_key_index = key_index.clone();
                    defined_key_indexes.add_key_index(&key_index);
                    if let parser::ValueKind::InlineTable(x) | parser::ValueKind::DottedTable(x) = &new_value.e {
                        immutable_key_indexes.add_key_index(&key_index);
                        for ((_, key), _) in x {
                            key_index.push(key.to_string());
                            immutable_key_indexes.add_key_index(&key_index);
                        }
                    }
                    add_to_table(&mut value, &top_key_index, toml_value_from_parser_value(new_value))?;
                }
            }
        }
    }
    Ok(value)
}
