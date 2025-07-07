use std::rc::Rc;

use rustc_hash::FxHashMap as HashMap;

use crate::evaluator::{Value, ValueInner};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CacheableValue {
    String(String),
    Number(u64), // f64 is not hashable, so we use its bit mode u64 representation
    Bool(bool),
    Nil,
    Reference(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CacheKey {
    func_id: usize, // ptr
    args: Vec<CacheableValue>,
}

pub type CallCache<'de> = HashMap<CacheKey, Value<'de>>;

pub fn build_cache_key(func_value: &Value, args: &[Value]) -> CacheKey {
    let func_id = Rc::as_ptr(&func_value.inner) as usize;

    let args = args
        .iter()
        .map(|arg| match &**arg {
            ValueInner::String(s) => CacheableValue::String(s.to_string()),
            ValueInner::Number(n) => CacheableValue::Number(n.to_bits()),
            ValueInner::Bool(b) => CacheableValue::Bool(*b),
            ValueInner::Nil => CacheableValue::Nil,
            ValueInner::Function(_)
            | ValueInner::NativeFunction(_)
            | ValueInner::Object(_)
            | ValueInner::Class(_) => CacheableValue::Reference(Rc::as_ptr(&arg.inner) as usize),
        })
        .collect();

    CacheKey { func_id, args }
}
