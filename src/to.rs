
use crate::TomlValue;
use std::convert::TryFrom;

use std::{collections::HashMap, path::PathBuf};

pub trait ToToml {
    fn to_toml(&self) -> Option<TomlValue>;
}

macro_rules! impl_to_toml_integer {
    ($x:ty) => {
        impl ToToml for $x {
            fn to_toml(&self) -> Option<TomlValue> {
                Some(TomlValue::Integer(*self as i64))
            }
        }
    }
}

impl_to_toml_integer!(u8);
impl_to_toml_integer!(u16);
impl_to_toml_integer!(u32);

impl ToToml for u64 {
    fn to_toml(&self) -> Option<TomlValue> {
        if let Some(y) = i64::try_from(*self).ok() {
            Some(TomlValue::Integer(y))
        } else {
            Some(TomlValue::String(format!("{}", self)))
        }
    }
}

impl ToToml for usize {
    fn to_toml(&self) -> Option<TomlValue> {
        if let Some(y) = i64::try_from(*self).ok() {
            Some(TomlValue::Integer(y))
        } else {
            Some(TomlValue::String(format!("{}", self)))
        }
    }
}

// impl_to_toml_integer!(u64); // TODO: this is incorrect, should use string as fallback when the integer can't be converted to i64
// impl_to_toml_integer!(usize);

impl_to_toml_integer!(i8);
impl_to_toml_integer!(i16);
impl_to_toml_integer!(i32);
impl_to_toml_integer!(i64);
impl_to_toml_integer!(isize);

impl ToToml for f32 {
    fn to_toml(&self) -> Option<TomlValue> {
        Some(TomlValue::Float(*self as f64))
    }
}

impl ToToml for f64 {
    fn to_toml(&self) -> Option<TomlValue> {
        Some(TomlValue::Float(*self))
    }
}

impl ToToml for bool {
    fn to_toml(&self) -> Option<TomlValue> {
        Some(TomlValue::Boolean(*self))
    }
}

impl ToToml for String {
    fn to_toml(&self) -> Option<TomlValue> {
        Some(TomlValue::String(self.clone()))
    }
}

impl<T> ToToml for Vec<T> where T: ToToml {
    fn to_toml(&self) -> Option<TomlValue> {
        Some(TomlValue::Array(self.into_iter().flat_map(|x| x.to_toml()).collect()))
    }
}

impl<V> ToToml for HashMap<String, V> where V: ToToml {
    fn to_toml(&self) -> Option<TomlValue> {
        let mut table = HashMap::new();
        for (key, value) in self {
            if let Some(value) = value.to_toml() {
                table.insert(key.clone(), value);
            }
        }
        Some(TomlValue::Table(table))
    }
}

impl<T> ToToml for Option<T> where T: ToToml {
    fn to_toml(&self) -> Option<TomlValue> {
        self.as_ref().map(|x| x.to_toml()).flatten()
    }
}

impl<T> ToToml for Box<T> where T: ToToml {
    fn to_toml(&self) -> Option<TomlValue> {
        self.as_ref().to_toml()
    }
}

impl ToToml for PathBuf {
    fn to_toml(&self) -> Option<TomlValue> {
        self.to_str().map(|s| TomlValue::String( s.to_string() ))
    }
}