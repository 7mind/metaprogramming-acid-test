use std::any::{Any, TypeId};
use std::fmt;

pub use functoid_macro::functoid;

pub mod subtyping;

/// Type information for runtime introspection
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TypeInfo {
    pub type_id: TypeId,
    pub type_name: &'static str,
}

impl TypeInfo {
    pub fn of<T: 'static>() -> Self {
        TypeInfo {
            type_id: TypeId::of::<T>(),
            type_name: std::any::type_name::<T>(),
        }
    }
}

impl fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeInfo({})", self.type_name)
    }
}

impl fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.type_name)
    }
}

/// Parameter information including optional identifier
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ParamInfo {
    pub type_info: TypeInfo,
    pub id: Option<&'static str>,
    pub name: &'static str,
}

impl ParamInfo {
    pub fn new(type_info: TypeInfo, name: &'static str) -> Self {
        ParamInfo {
            type_info,
            id: None,
            name,
        }
    }

    pub fn with_id(type_info: TypeInfo, name: &'static str, id: &'static str) -> Self {
        ParamInfo {
            type_info,
            id: Some(id),
            name,
        }
    }
}

impl fmt::Debug for ParamInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(id) = self.id {
            write!(f, "{}:{} @Id({})", self.name, self.type_info.type_name, id)
        } else {
            write!(f, "{}:{}", self.name, self.type_info.type_name)
        }
    }
}

impl fmt::Display for ParamInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Trait for runtime-introspectable functions
pub trait Functoid {
    type Output: 'static;

    /// Get information about all parameters
    fn param_info(&self) -> &[ParamInfo];

    /// Get type information for the return type
    fn return_type(&self) -> TypeInfo;

    /// Invoke the function with boxed arguments
    /// Arguments must be in the same order as param_info
    /// Panics if argument types don't match or wrong number of arguments
    fn invoke(&self, args: Vec<Box<dyn Any>>) -> Self::Output;
}

/// Helper trait to convert values into boxed Any
pub trait IntoBoxedAny {
    fn into_boxed_any(self) -> Box<dyn Any>;
}

impl<T: 'static> IntoBoxedAny for T {
    fn into_boxed_any(self) -> Box<dyn Any> {
        Box::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_info() {
        let ti1 = TypeInfo::of::<i32>();
        let ti2 = TypeInfo::of::<i32>();
        let ti3 = TypeInfo::of::<String>();

        assert_eq!(ti1, ti2);
        assert_ne!(ti1, ti3);
        assert_eq!(ti1.type_name, "i32");
    }

    #[test]
    fn test_param_info() {
        let pi1 = ParamInfo::new(TypeInfo::of::<i32>(), "x");
        assert_eq!(pi1.name, "x");
        assert_eq!(pi1.id, None);
        assert_eq!(pi1.type_info.type_name, "i32");

        let pi2 = ParamInfo::with_id(TypeInfo::of::<String>(), "name", "user-name");
        assert_eq!(pi2.name, "name");
        assert_eq!(pi2.id, Some("user-name"));
        assert_eq!(pi2.type_info.type_name, "alloc::string::String");
    }
}
