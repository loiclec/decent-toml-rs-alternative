
use crate::TomlValue;

use std::{collections::HashMap, convert::TryFrom, path::PathBuf};

pub trait FromToml : Sized {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self>;
}

macro_rules! impl_from_toml_integer {
    ($x:ty) => {
        impl FromToml for $x {
            fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
                match from {
                    Some(TomlValue::Integer(x)) => {
                        <$x>::try_from(*x).ok()
                    }
                    _ => { None }
                }
            }
        }
    };
}

impl_from_toml_integer!(u8);
impl_from_toml_integer!(u16);
impl_from_toml_integer!(u32);
impl_from_toml_integer!(u64); // TODO: this is incorrect, should use string as fallback when the integer can't be converted to i64
impl_from_toml_integer!(usize);

impl_from_toml_integer!(i8);
impl_from_toml_integer!(i16);
impl_from_toml_integer!(i32);
impl_from_toml_integer!(i64);
impl_from_toml_integer!(isize);

impl FromToml for f32 {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        match from {
            Some(TomlValue::Float(x)) => {
                Some(*x as f32)
            }
            _ => { None }
        }
    }
}

impl FromToml for f64 {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        match from {
            Some(TomlValue::Float(x)) => {
                Some(*x)
            }
            _ => { None }
        }
    }
}
impl FromToml for bool {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        match from {
            Some(TomlValue::Boolean(x)) => {
                Some(*x)
            }
            _ => { None }
        }
    }
}

impl FromToml for String {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        match from {
            Some(TomlValue::String(x)) => {
                Some(x.clone())
            }
            _ => { None }
        }
    }
}
impl<T> FromToml for Vec<T> where T: FromToml {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        match from {
            Some(TomlValue::Array(array)) => {
                let mut res = Self::new();
                for x in array {
                    if let Some(x) = T::from_toml(Some(x)) {
                        res.push(x);
                    } else {
                        return None
                    }
                }
                Some(res)
            }
            _ => {
                None
            }
        }
    }
}

impl<V> FromToml for HashMap<String, V> where V: FromToml {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        match from {
            Some(TomlValue::Table(table)) => {
                let mut res = Self::new();
                for (key, value) in table {
                    if let Some(value) = V::from_toml(Some(value)) {
                        res.insert(key.clone(), value);
                    } else {
                        return None
                    }
                }
                Some(res)
            }
            _ => {
                None
            }
        }
    }
}

impl<T> FromToml for Option<T> where T: FromToml {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        if let Some(from) = from {
            if let Some(value) = T::from_toml(Some(from)) {
                Some(Some(value))
            } else {
                None
            }
        } else {
            Some(None)
        }
        
    }
}

impl<T> FromToml for Box<T> where T: FromToml {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        T::from_toml(from).map(Box::new)
    }
}

impl FromToml for PathBuf {
    fn from_toml(from: Option<&TomlValue>) -> Option<Self> {
        if let Some(TomlValue::String(s)) = from {
            Some(PathBuf::from(s))
        } else {
            None
        }
    }
}