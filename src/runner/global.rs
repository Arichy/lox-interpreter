use std::{cell::OnceCell, collections::HashMap};

use chrono::Utc;
use miette::Error;

use crate::evaluator::Value;

pub fn clock<'de>(arguments: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let now = Utc::now().timestamp();
    Ok(Value::new_number(now as f64))
}

pub mod console {
    use super::*;

    pub fn log<'de>(arguments: &[Value<'de>]) -> Result<Value<'de>, Error> {
        for arg in arguments {
            println!("{}", arg);
        }
        Ok(Value::new_nil())
    }
}

pub struct Console {
    pub log: for<'de> fn(&[Value<'de>]) -> Result<Value<'de>, Error>,
}
impl Console {
    pub fn new<'de>() -> Value<'de> {
        Value::new_object(HashMap::from([(
            "log".to_string(),
            Value::new_native_function("log".to_string(), console::log),
        )]))
    }
}
