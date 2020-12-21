
// File based on code from Alex Crichton’s toml-rs library
// https://github.com/alexcrichton/toml-rs/
// Copyright (c) 2014 Alex Crichton

//! Deserializing TOML into Rust structures.
//!
//! This module contains all the Serde support for deserializing TOML documents
//! into Rust structures. Note that some top-level functions here are also
//! provided at the top of the crate.

use std::borrow::Cow;
use std::error;
use std::f64;
use std::fmt;
use std::str;

use crate::tokens::{Error as TokenError, Span, Token, Tokenizer};

/// Type Alias for a TOML Table pair
type TablePair<'a> = ((Span, Cow<'a, str>), Value<'a>);

/// Errors that can occur when deserializing a type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    inner: Box<ErrorInner>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ErrorInner {
    kind: ErrorKind,
    line: Option<usize>,
    col: usize,
    at: Option<usize>,
    message: String,
    key: Vec<String>,
}

/// Errors that can occur when deserializing a type.
#[derive(Debug, PartialEq, Eq, Clone)]
enum ErrorKind {
    /// EOF was reached when looking for a value
    UnexpectedEof,

    /// An invalid character not allowed in a string was found
    InvalidCharInString(char),

    /// An invalid character was found as an escape
    InvalidEscape(char),

    /// An invalid character was found in a hex escape
    InvalidHexEscape(char),

    /// An invalid escape value was specified in a hex escape in a string.
    ///
    /// Valid values are in the plane of unicode codepoints.
    InvalidEscapeValue(u32),

    /// A newline in a string was encountered when one was not allowed.
    NewlineInString,

    /// An unexpected character was encountered, typically when looking for a
    /// value.
    Unexpected(char),

    /// An unterminated string was found where EOF was found before the ending
    /// EOF mark.
    UnterminatedString,

    /// A newline was found in a table key.
    NewlineInTableKey,

    /// A number failed to parse
    NumberInvalid,

    /// A date or datetime was invalid
    DateInvalid,

    /// Wanted one sort of token, but found another.
    Wanted {
        /// Expected token type
        expected: &'static str,
        /// Actually found token type
        found: &'static str,
    },

    /// An empty table key was found.
    EmptyTableKey,

    /// Multiline strings are not allowed for key
    MultilineStringKey,

    /// Dotted key attempted to extend something that is not a table.
    DottedKeyInvalidType,

    #[doc(hidden)]
    __Nonexhaustive,
}

/// Deserialization implementation for TOML.
pub struct Deserializer<'a> {
    input: &'a str,
    tokens: Tokenizer<'a>,
}

#[derive(Debug)]
struct Table<'a> {
    at: usize,
    header: Vec<(Span, Cow<'a, str>)>,
    values: Option<Vec<TablePair<'a>>>,
    array: bool,
}

impl<'a> Deserializer<'a> {
    /// Creates a new deserializer which will be deserializing the string
    /// provided.
    pub fn new(input: &'a str) -> Deserializer<'a> {
        Deserializer {
            tokens: Tokenizer::new(input),
            input,
        }
    }

    /// The `Deserializer::end` method should be called after a value has been
    /// fully deserialized.  This allows the `Deserializer` to validate that the
    /// input stream is at the end or that it only has trailing
    /// whitespace/comments.
    pub fn end(&mut self) -> Result<(), Error> {
        Ok(())
    }

    pub fn line(&mut self) -> Result<Option<Line<'a>>, Error> {
        // TODO: parse comments
        loop {
            self.eat_whitespace()?;
            if self.eat_comment()? {
                continue;
            }
            if self.eat(Token::Newline)? {
                continue;
            }
            break;
        }

        match self.peek()? {
            Some((_, Token::LeftBracket)) => self.table_header().map(Some),
            Some(_) => self.key_value().map(Some),
            None => Ok(None),
        }
    }

    fn table_header(&mut self) -> Result<Line<'a>, Error> {
        let start = self.tokens.current();
        self.expect(Token::LeftBracket)?;
        let array = self.eat(Token::LeftBracket)?;
        let ret = Header::new(self.tokens.clone(), array);
        self.tokens.skip_to_newline();
        Ok(Line::Table {
            at: start,
            header: ret,
            array,
        })
    }

    fn key_value(&mut self) -> Result<Line<'a>, Error> {
        let key = self.dotted_key()?;
        self.eat_whitespace()?;
        self.expect(Token::Equals)?;
        self.eat_whitespace()?;

        let value = self.value()?;
        self.eat_whitespace()?;
        if !self.eat_comment()? {
            self.eat_newline_or_eof()?;
        }

        Ok(Line::KeyValue(key, value))
    }

    fn value(&mut self) -> Result<Value<'a>, Error> {
        let at = self.tokens.current();
        let value = match self.next()? {
            Some((Span { start, end }, Token::String { val, .. })) => Value {
                e: ValueKind::String(val),
                start,
                end,
            },
            Some((Span { start, end }, Token::Keylike("true"))) => Value {
                e: ValueKind::Boolean(true),
                start,
                end,
            },
            Some((Span { start, end }, Token::Keylike("false"))) => Value {
                e: ValueKind::Boolean(false),
                start,
                end,
            },
            Some((span, Token::Keylike(key))) => self.number_or_date(span, key)?,
            Some((span, Token::Plus)) => self.number_leading_plus(span)?,
            Some((Span { start, .. }, Token::LeftBrace)) => {
                self.inline_table().map(|(Span { end, .. }, table)| Value {
                    e: ValueKind::InlineTable(table),
                    start,
                    end,
                })?
            }
            Some((Span { start, .. }, Token::LeftBracket)) => {
                self.array().map(|(Span { end, .. }, array)| Value {
                    e: ValueKind::Array(array),
                    start,
                    end,
                })?
            }
            Some(token) => {
                return Err(self.error(
                    at,
                    ErrorKind::Wanted {
                        expected: "a value",
                        found: token.1.describe(),
                    },
                ))
            }
            None => return Err(self.eof()),
        };
        Ok(value)
    }

    fn number_or_date(&mut self, span: Span, s: &'a str) -> Result<Value<'a>, Error> {
        if s.contains('T')
            || s.contains('t')
            || (s.len() > 1 && s[1..].contains('-') && !s.contains("e-") && !s.contains("E-"))
        {
            self.datetime(span, s, false)
                .map(|(Span { start, end }, d)| Value {
                    e: ValueKind::Datetime(d),
                    start,
                    end,
                })
        } else if self.eat(Token::Colon)? {
            self.datetime(span, s, true)
                .map(|(Span { start, end }, d)| Value {
                    e: ValueKind::Datetime(d),
                    start,
                    end,
                })
        } else {
            self.number(span, s)
        }
    }

    fn number(&mut self, Span { start, end }: Span, s: &'a str) -> Result<Value<'a>, Error> {
        let to_integer = |f| Value {
            e: ValueKind::Integer(f),
            start,
            end,
        };
        if s.starts_with("0x") {
            self.integer(&s[2..], 16).map(to_integer)
        } else if s.starts_with("0o") {
            self.integer(&s[2..], 8).map(to_integer)
        } else if s.starts_with("0b") {
            self.integer(&s[2..], 2).map(to_integer)
        } else if s.contains('e') || s.contains('E') {
            self.float(s, None).map(|f| Value {
                e: ValueKind::Float(f),
                start,
                end,
            })
        } else if self.eat(Token::Period)? {
            let at = self.tokens.current();
            match self.next()? {
                Some((Span { start, end }, Token::Keylike(after))) => {
                    self.float(s, Some(after)).map(|f| Value {
                        e: ValueKind::Float(f),
                        start,
                        end,
                    })
                }
                _ => Err(self.error(at, ErrorKind::NumberInvalid)),
            }
        } else if s == "inf" {
            Ok(Value {
                e: ValueKind::Float(f64::INFINITY),
                start,
                end,
            })
        } else if s == "-inf" {
            Ok(Value {
                e: ValueKind::Float(f64::NEG_INFINITY),
                start,
                end,
            })
        } else if s == "nan" {
            Ok(Value {
                e: ValueKind::Float(f64::NAN),
                start,
                end,
            })
        } else if s == "-nan" {
            Ok(Value {
                e: ValueKind::Float(-f64::NAN),
                start,
                end,
            })
        } else {
            self.integer(s, 10).map(to_integer)
        }
    }

    fn number_leading_plus(&mut self, Span { start, .. }: Span) -> Result<Value<'a>, Error> {
        let start_token = self.tokens.current();
        match self.next()? {
            Some((Span { end, .. }, Token::Keylike(s))) => self.number(Span { start, end }, s),
            _ => Err(self.error(start_token, ErrorKind::NumberInvalid)),
        }
    }

    fn integer(&self, s: &'a str, radix: u32) -> Result<i64, Error> {
        let allow_sign = radix == 10;
        let allow_leading_zeros = radix != 10;
        let (prefix, suffix) = self.parse_integer(s, allow_sign, allow_leading_zeros, radix)?;
        let start = self.tokens.substr_offset(s);
        if suffix != "" {
            return Err(self.error(start, ErrorKind::NumberInvalid));
        }
        i64::from_str_radix(&prefix.replace("_", "").trim_start_matches('+'), radix)
            .map_err(|_e| self.error(start, ErrorKind::NumberInvalid))
    }

    fn parse_integer(
        &self,
        s: &'a str,
        allow_sign: bool,
        allow_leading_zeros: bool,
        radix: u32,
    ) -> Result<(&'a str, &'a str), Error> {
        let start = self.tokens.substr_offset(s);

        let mut first = true;
        let mut first_zero = false;
        let mut underscore = false;
        let mut end = s.len();
        for (i, c) in s.char_indices() {
            let at = i + start;
            if i == 0 && (c == '+' || c == '-') && allow_sign {
                continue;
            }

            if c == '0' && first {
                first_zero = true;
            } else if c.is_digit(radix) {
                if !first && first_zero && !allow_leading_zeros {
                    return Err(self.error(at, ErrorKind::NumberInvalid));
                }
                underscore = false;
            } else if c == '_' && first {
                return Err(self.error(at, ErrorKind::NumberInvalid));
            } else if c == '_' && !underscore {
                underscore = true;
            } else {
                end = i;
                break;
            }
            first = false;
        }
        if first || underscore {
            return Err(self.error(start, ErrorKind::NumberInvalid));
        }
        Ok((&s[..end], &s[end..]))
    }

    fn float(&mut self, s: &'a str, after_decimal: Option<&'a str>) -> Result<f64, Error> {
        let (integral, mut suffix) = self.parse_integer(s, true, false, 10)?;
        let start = self.tokens.substr_offset(integral);

        let mut fraction = None;
        if let Some(after) = after_decimal {
            if suffix != "" {
                return Err(self.error(start, ErrorKind::NumberInvalid));
            }
            let (a, b) = self.parse_integer(after, false, true, 10)?;
            fraction = Some(a);
            suffix = b;
        }

        let mut exponent = None;
        if suffix.starts_with('e') || suffix.starts_with('E') {
            let (a, b) = if suffix.len() == 1 {
                self.eat(Token::Plus)?;
                match self.next()? {
                    Some((_, Token::Keylike(s))) => self.parse_integer(s, false, true, 10)?,
                    _ => return Err(self.error(start, ErrorKind::NumberInvalid)),
                }
            } else {
                self.parse_integer(&suffix[1..], true, true, 10)?
            };
            if b != "" {
                return Err(self.error(start, ErrorKind::NumberInvalid));
            }
            exponent = Some(a);
        } else if !suffix.is_empty() {
            return Err(self.error(start, ErrorKind::NumberInvalid));
        }

        let mut number = integral
            .trim_start_matches('+')
            .chars()
            .filter(|c| *c != '_')
            .collect::<String>();
        if let Some(fraction) = fraction {
            number.push_str(".");
            number.extend(fraction.chars().filter(|c| *c != '_'));
        }
        if let Some(exponent) = exponent {
            number.push_str("E");
            number.extend(exponent.chars().filter(|c| *c != '_'));
        }
        number
            .parse()
            .map_err(|_e| self.error(start, ErrorKind::NumberInvalid))
            .and_then(|n: f64| {
                if n.is_finite() {
                    Ok(n)
                } else {
                    Err(self.error(start, ErrorKind::NumberInvalid))
                }
            })
    }

    fn datetime(
        &mut self,
        mut span: Span,
        date: &'a str,
        colon_eaten: bool,
    ) -> Result<(Span, &'a str), Error> {
        let start = self.tokens.substr_offset(date);

        // Check for space separated date and time.
        let mut lookahead = self.tokens.clone();
        if let Ok(Some((_, Token::Whitespace(" ")))) = lookahead.next() {
            // Check if hour follows.
            if let Ok(Some((_, Token::Keylike(_)))) = lookahead.next() {
                self.next()?; // skip space
                self.next()?; // skip keylike hour
            }
        }

        if colon_eaten || self.eat(Token::Colon)? {
            // minutes
            match self.next()? {
                Some((_, Token::Keylike(_))) => {}
                _ => return Err(self.error(start, ErrorKind::DateInvalid)),
            }
            // Seconds
            self.expect(Token::Colon)?;
            match self.next()? {
                Some((Span { end, .. }, Token::Keylike(_))) => {
                    span.end = end;
                }
                _ => return Err(self.error(start, ErrorKind::DateInvalid)),
            }
            // Fractional seconds
            if self.eat(Token::Period)? {
                match self.next()? {
                    Some((Span { end, .. }, Token::Keylike(_))) => {
                        span.end = end;
                    }
                    _ => return Err(self.error(start, ErrorKind::DateInvalid)),
                }
            }

            // offset
            if self.eat(Token::Plus)? {
                match self.next()? {
                    Some((Span { end, .. }, Token::Keylike(_))) => {
                        span.end = end;
                    }
                    _ => return Err(self.error(start, ErrorKind::DateInvalid)),
                }
            }
            if self.eat(Token::Colon)? {
                match self.next()? {
                    Some((Span { end, .. }, Token::Keylike(_))) => {
                        span.end = end;
                    }
                    _ => return Err(self.error(start, ErrorKind::DateInvalid)),
                }
            }
        }

        let end = self.tokens.current();
        Ok((span, &self.tokens.input()[start..end]))
    }

    // TODO(#140): shouldn't buffer up this entire table in memory, it'd be
    // great to defer parsing everything until later.
    fn inline_table(&mut self) -> Result<(Span, Vec<TablePair<'a>>), Error> {
        let mut ret = Vec::new();
        self.eat_whitespace()?;
        if let Some(span) = self.eat_spanned(Token::RightBrace)? {
            return Ok((span, ret));
        }
        loop {
            let key = self.dotted_key()?;
            self.eat_whitespace()?;
            self.expect(Token::Equals)?;
            self.eat_whitespace()?;
            let value = self.value()?;
            self.add_dotted_key(key, value, &mut ret)?;

            self.eat_whitespace()?;
            if let Some(span) = self.eat_spanned(Token::RightBrace)? {
                return Ok((span, ret));
            }
            self.expect(Token::Comma)?;
            self.eat_whitespace()?;
        }
    }

    // TODO(#140): shouldn't buffer up this entire array in memory, it'd be
    // great to defer parsing everything until later.
    pub fn array(&mut self) -> Result<(Span, Vec<Value<'a>>), Error> {
        let mut ret = Vec::new();

        let intermediate = |me: &mut Deserializer<'_>| {
            loop {
                me.eat_whitespace()?;
                if !me.eat(Token::Newline)? && !me.eat_comment()? {
                    break;
                }
            }
            Ok(())
        };

        loop {
            intermediate(self)?;
            if let Some(span) = self.eat_spanned(Token::RightBracket)? {
                return Ok((span, ret));
            }
            let value = self.value()?;
            ret.push(value);
            intermediate(self)?;
            if !self.eat(Token::Comma)? {
                break;
            }
        }
        intermediate(self)?;
        let span = self.expect_spanned(Token::RightBracket)?;
        Ok((span, ret))
    }

    fn table_key(&mut self) -> Result<(Span, Cow<'a, str>), Error> {
        self.tokens.table_key().map_err(|e| self.token_error(e))
    }

    fn dotted_key(&mut self) -> Result<Vec<(Span, Cow<'a, str>)>, Error> {
        let mut result = Vec::new();
        result.push(self.table_key()?);
        self.eat_whitespace()?;
        while self.eat(Token::Period)? {
            self.eat_whitespace()?;
            result.push(self.table_key()?);
            self.eat_whitespace()?;
        }
        Ok(result)
    }

    /// Stores a value in the appropriate hierachical structure positioned based on the dotted key.
    ///
    /// Given the following definition: `multi.part.key = "value"`, `multi` and `part` are
    /// intermediate parts which are mapped to the relevant fields in the deserialized type's data
    /// hierarchy.
    ///
    /// # Parameters
    ///
    /// * `key_parts`: Each segment of the dotted key, e.g. `part.one` maps to
    ///                `vec![Cow::Borrowed("part"), Cow::Borrowed("one")].`
    /// * `value`: The parsed value.
    /// * `values`: The `Vec` to store the value in.
    fn add_dotted_key(
        &self,
        mut key_parts: Vec<(Span, Cow<'a, str>)>,
        value: Value<'a>,
        values: &mut Vec<TablePair<'a>>,
    ) -> Result<(), Error> {
        let key = key_parts.remove(0);
        if key_parts.is_empty() {
            values.push((key, value));
            return Ok(());
        }
        match values.iter_mut().find(|&&mut (ref k, _)| *k.1 == key.1) {
            Some(&mut (
                _,
                Value {
                    e: ValueKind::DottedTable(ref mut v),
                    ..
                },
            )) => {
                return self.add_dotted_key(key_parts, value, v);
            }
            Some(&mut (_, Value { start, .. })) => {
                return Err(self.error(start, ErrorKind::DottedKeyInvalidType));
            }
            None => {}
        }
        // The start/end value is somewhat misleading here.
        let table_values = Value {
            e: ValueKind::DottedTable(Vec::new()),
            start: value.start,
            end: value.end,
        };
        values.push((key, table_values));
        let last_i = values.len() - 1;
        if let (
            _,
            Value {
                e: ValueKind::DottedTable(ref mut v),
                ..
            },
        ) = values[last_i]
        {
            self.add_dotted_key(key_parts, value, v)?;
        }
        Ok(())
    }

    fn eat_whitespace(&mut self) -> Result<(), Error> {
        self.tokens
            .eat_whitespace()
            .map_err(|e| self.token_error(e))
    }

    fn eat_comment(&mut self) -> Result<bool, Error> {
        self.tokens.eat_comment().map_err(|e| self.token_error(e))
    }

    fn eat_newline_or_eof(&mut self) -> Result<(), Error> {
        self.tokens
            .eat_newline_or_eof()
            .map_err(|e| self.token_error(e))
    }

    fn eat(&mut self, expected: Token<'a>) -> Result<bool, Error> {
        self.tokens.eat(expected).map_err(|e| self.token_error(e))
    }

    fn eat_spanned(&mut self, expected: Token<'a>) -> Result<Option<Span>, Error> {
        self.tokens
            .eat_spanned(expected)
            .map_err(|e| self.token_error(e))
    }

    fn expect(&mut self, expected: Token<'a>) -> Result<(), Error> {
        self.tokens
            .expect(expected)
            .map_err(|e| self.token_error(e))
    }

    fn expect_spanned(&mut self, expected: Token<'a>) -> Result<Span, Error> {
        self.tokens
            .expect_spanned(expected)
            .map_err(|e| self.token_error(e))
    }

    fn next(&mut self) -> Result<Option<(Span, Token<'a>)>, Error> {
        self.tokens.next().map_err(|e| self.token_error(e))
    }

    fn peek(&mut self) -> Result<Option<(Span, Token<'a>)>, Error> {
        self.tokens.peek().map_err(|e| self.token_error(e))
    }

    fn eof(&self) -> Error {
        self.error(self.input.len(), ErrorKind::UnexpectedEof)
    }

    fn token_error(&self, error: TokenError) -> Error {
        match error {
            TokenError::InvalidCharInString(at, ch) => {
                self.error(at, ErrorKind::InvalidCharInString(ch))
            }
            TokenError::InvalidEscape(at, ch) => self.error(at, ErrorKind::InvalidEscape(ch)),
            TokenError::InvalidEscapeValue(at, v) => {
                self.error(at, ErrorKind::InvalidEscapeValue(v))
            }
            TokenError::InvalidHexEscape(at, ch) => self.error(at, ErrorKind::InvalidHexEscape(ch)),
            TokenError::NewlineInString(at) => self.error(at, ErrorKind::NewlineInString),
            TokenError::Unexpected(at, ch) => self.error(at, ErrorKind::Unexpected(ch)),
            TokenError::UnterminatedString(at) => self.error(at, ErrorKind::UnterminatedString),
            TokenError::NewlineInTableKey(at) => self.error(at, ErrorKind::NewlineInTableKey),
            TokenError::Wanted {
                at,
                expected,
                found,
            } => self.error(at, ErrorKind::Wanted { expected, found }),
            TokenError::EmptyTableKey(at) => self.error(at, ErrorKind::EmptyTableKey),
            TokenError::MultilineStringKey(at) => self.error(at, ErrorKind::MultilineStringKey),
        }
    }

    fn error(&self, at: usize, kind: ErrorKind) -> Error {
        let mut err = Error::from_kind(Some(at), kind);
        err.fix_linecol(|at| self.to_linecol(at));
        err
    }

    /// Converts a byte offset from an error message to a (line, column) pair
    ///
    /// All indexes are 0-based.
    fn to_linecol(&self, offset: usize) -> (usize, usize) {
        let mut cur = 0;
        // Use split_terminator instead of lines so that if there is a `\r`,
        // it is included in the offset calculation. The `+1` values below
        // account for the `\n`.
        for (i, line) in self.input.split_terminator('\n').enumerate() {
            if cur + line.len() + 1 > offset {
                return (i, offset - cur);
            }
            cur += line.len() + 1;
        }
        (self.input.lines().count(), 0)
    }
}

impl Error {
    /// Produces a (line, column) pair of the position of the error if available
    ///
    /// All indexes are 0-based.
    pub fn line_col(&self) -> Option<(usize, usize)> {
        self.inner.line.map(|line| (line, self.inner.col))
    }

    fn from_kind(at: Option<usize>, kind: ErrorKind) -> Error {
        Error {
            inner: Box::new(ErrorInner {
                kind,
                line: None,
                col: 0,
                at,
                message: String::new(),
                key: Vec::new(),
            }),
        }
    }

    fn fix_linecol<F>(&mut self, f: F)
    where
        F: FnOnce(usize) -> (usize, usize),
    {
        if let Some(at) = self.inner.at {
            let (line, col) = f(at);
            self.inner.line = Some(line);
            self.inner.col = col;
        }
    }
}

impl std::convert::From<Error> for std::io::Error {
    fn from(e: Error) -> Self {
        std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.kind {
            ErrorKind::UnexpectedEof => "unexpected eof encountered".fmt(f)?,
            ErrorKind::InvalidCharInString(c) => write!(
                f,
                "invalid character in string: `{}`",
                c.escape_default().collect::<String>()
            )?,
            ErrorKind::InvalidEscape(c) => write!(
                f,
                "invalid escape character in string: `{}`",
                c.escape_default().collect::<String>()
            )?,
            ErrorKind::InvalidHexEscape(c) => write!(
                f,
                "invalid hex escape character in string: `{}`",
                c.escape_default().collect::<String>()
            )?,
            ErrorKind::InvalidEscapeValue(c) => write!(f, "invalid escape value: `{}`", c)?,
            ErrorKind::NewlineInString => "newline in string found".fmt(f)?,
            ErrorKind::Unexpected(ch) => write!(
                f,
                "unexpected character found: `{}`",
                ch.escape_default().collect::<String>()
            )?,
            ErrorKind::UnterminatedString => "unterminated string".fmt(f)?,
            ErrorKind::NewlineInTableKey => "found newline in table key".fmt(f)?,
            ErrorKind::Wanted { expected, found } => {
                write!(f, "expected {}, found {}", expected, found)?
            }
            ErrorKind::NumberInvalid => "invalid number".fmt(f)?,
            ErrorKind::DateInvalid => "invalid date".fmt(f)?,
            ErrorKind::EmptyTableKey => "empty table key found".fmt(f)?,
            ErrorKind::MultilineStringKey => "multiline strings are not allowed for key".fmt(f)?,
            ErrorKind::DottedKeyInvalidType => {
                "dotted key attempted to extend non-table type".fmt(f)?
            },
            ErrorKind::__Nonexhaustive => panic!(),
        }

        if !self.inner.key.is_empty() {
            write!(f, " for key `")?;
            for (i, k) in self.inner.key.iter().enumerate() {
                if i > 0 {
                    write!(f, ".")?;
                }
                write!(f, "{}", k)?;
            }
            write!(f, "`")?;
        }

        if let Some(line) = self.inner.line {
            write!(f, " at line {} column {}", line + 1, self.inner.col + 1)?;
        }

        Ok(())
    }
}

impl error::Error for Error {}

pub enum Line<'a> {
    Table {
        at: usize,
        header: Header<'a>,
        array: bool,
    },
    KeyValue(Vec<(Span, Cow<'a, str>)>, Value<'a>),
}

pub struct Header<'a> {
    first: bool,
    pub array: bool,
    tokens: Tokenizer<'a>,
}

impl<'a> Header<'a> {
    fn new(tokens: Tokenizer<'a>, array: bool) -> Header<'a> {
        Header {
            first: true,
            array,
            tokens,
        }
    }

    pub fn next(&mut self) -> Result<Option<(Span, Cow<'a, str>)>, TokenError> {
        self.tokens.eat_whitespace()?;

        if self.first || self.tokens.eat(Token::Period)? {
            self.first = false;
            self.tokens.eat_whitespace()?;
            self.tokens.table_key().map(|t| t).map(Some)
        } else {
            self.tokens.expect(Token::RightBracket)?;
            if self.array {
                self.tokens.expect(Token::RightBracket)?;
            }

            self.tokens.eat_whitespace()?;
            if !self.tokens.eat_comment()? {
                self.tokens.eat_newline_or_eof()?;
            }
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub struct Value<'a> {
    pub e: ValueKind<'a>,
    start: usize,
    end: usize,
}

#[derive(Debug)]
pub enum ValueKind<'a> {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(Cow<'a, str>),
    Datetime(&'a str),
    Array(Vec<Value<'a>>),
    InlineTable(Vec<TablePair<'a>>),
    DottedTable(Vec<TablePair<'a>>),
}

// impl<'a> E<'a> {
//     fn type_name(&self) -> &'static str {
//         match *self {
//             E::String(..) => "string",
//             E::Integer(..) => "integer",
//             E::Float(..) => "float",
//             E::Boolean(..) => "boolean",
//             E::Datetime(..) => "datetime",
//             E::Array(..) => "array",
//             E::InlineTable(..) => "inline table",
//             E::DottedTable(..) => "dotted table",
//         }
//     }
// }
