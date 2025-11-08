//! Runtime trait/subtyping evidence similar to izumi-reflect's <:<
//!
//! Since Rust's TypeId doesn't provide subtyping information, we build
//! a registry that tracks which types implement which traits.

use std::any::TypeId;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

/// A unique identifier for a trait
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraitId {
    type_id: TypeId,
    name: &'static str,
}

impl TraitId {
    /// Create a TraitId for a trait type
    pub fn of<T: ?Sized + 'static>() -> Self {
        TraitId {
            type_id: TypeId::of::<T>(),
            name: std::any::type_name::<T>(),
        }
    }
}

impl std::fmt::Display for TraitId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Evidence that type T implements trait Tr
/// Similar to Scala's <:<[T, Tr]
#[derive(Clone)]
pub struct ImplEvidence {
    concrete_type: TypeId,
    concrete_name: &'static str,
    trait_id: TraitId,
}

impl ImplEvidence {
    pub fn new<T: 'static, Tr: ?Sized + 'static>() -> Self {
        ImplEvidence {
            concrete_type: TypeId::of::<T>(),
            concrete_name: std::any::type_name::<T>(),
            trait_id: TraitId::of::<Tr>(),
        }
    }

    pub fn concrete_type(&self) -> TypeId {
        self.concrete_type
    }

    pub fn trait_id(&self) -> TraitId {
        self.trait_id
    }
}

impl std::fmt::Debug for ImplEvidence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <: {}", self.concrete_name, self.trait_id)
    }
}

impl std::fmt::Display for ImplEvidence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Global registry tracking which types implement which traits
pub struct TraitRegistry {
    // TypeId -> Set of TraitIds it implements
    implementations: RwLock<HashMap<TypeId, HashSet<TraitId>>>,
    // TraitId -> Set of TypeIds that implement it
    implementors: RwLock<HashMap<TraitId, HashSet<TypeId>>>,
}

impl TraitRegistry {
    pub fn new() -> Self {
        TraitRegistry {
            implementations: RwLock::new(HashMap::new()),
            implementors: RwLock::new(HashMap::new()),
        }
    }

    /// Register that type T implements trait Tr
    pub fn register<T: 'static, Tr: ?Sized + 'static>(&self) {
        let type_id = TypeId::of::<T>();
        let trait_id = TraitId::of::<Tr>();

        // Add to implementations map
        self.implementations
            .write()
            .unwrap()
            .entry(type_id)
            .or_insert_with(HashSet::new)
            .insert(trait_id);

        // Add to implementors map
        self.implementors
            .write()
            .unwrap()
            .entry(trait_id)
            .or_insert_with(HashSet::new)
            .insert(type_id);
    }

    /// Check if type T implements trait Tr (similar to <:<)
    pub fn implements<T: 'static, Tr: ?Sized + 'static>(&self) -> bool {
        let type_id = TypeId::of::<T>();
        let trait_id = TraitId::of::<Tr>();

        self.implementations
            .read()
            .unwrap()
            .get(&type_id)
            .map(|traits| traits.contains(&trait_id))
            .unwrap_or(false)
    }

    /// Check if a TypeId implements a TraitId
    pub fn type_implements(&self, type_id: TypeId, trait_id: TraitId) -> bool {
        self.implementations
            .read()
            .unwrap()
            .get(&type_id)
            .map(|traits| traits.contains(&trait_id))
            .unwrap_or(false)
    }

    /// Get all traits implemented by a type
    pub fn traits_of(&self, type_id: TypeId) -> Vec<TraitId> {
        self.implementations
            .read()
            .unwrap()
            .get(&type_id)
            .map(|traits| traits.iter().copied().collect())
            .unwrap_or_default()
    }

    /// Get all types implementing a trait
    pub fn implementors_of(&self, trait_id: TraitId) -> Vec<TypeId> {
        self.implementors
            .read()
            .unwrap()
            .get(&trait_id)
            .map(|types| types.iter().copied().collect())
            .unwrap_or_default()
    }

    /// Get evidence that T implements Tr, if it does
    pub fn get_evidence<T: 'static, Tr: ?Sized + 'static>(&self) -> Option<ImplEvidence> {
        if self.implements::<T, Tr>() {
            Some(ImplEvidence::new::<T, Tr>())
        } else {
            None
        }
    }
}

impl Default for TraitRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Global trait registry instance
pub fn global_trait_registry() -> &'static Arc<TraitRegistry> {
    use std::sync::OnceLock;
    static REGISTRY: OnceLock<Arc<TraitRegistry>> = OnceLock::new();
    REGISTRY.get_or_init(|| Arc::new(TraitRegistry::new()))
}

/// Convenience macro to register trait implementations
#[macro_export]
macro_rules! register_impl {
    ($type:ty: $trait:ty) => {
        $crate::subtyping::global_trait_registry().register::<$type, $trait>();
    };
}

/// Check if T implements Tr at runtime (like <:<)
pub fn implements<T: 'static, Tr: ?Sized + 'static>() -> bool {
    global_trait_registry().implements::<T, Tr>()
}

/// Get subtyping evidence for T <: Tr
pub fn evidence<T: 'static, Tr: ?Sized + 'static>() -> Option<ImplEvidence> {
    global_trait_registry().get_evidence::<T, Tr>()
}

#[cfg(test)]
mod tests {
    use super::*;

    trait Animal {}
    trait Mammal: Animal {}

    struct Dog;
    struct Cat;

    impl Animal for Dog {}
    impl Animal for Cat {}
    impl Mammal for Dog {}

    #[test]
    fn test_trait_registration() {
        let registry = TraitRegistry::new();

        registry.register::<Dog, dyn Animal>();
        registry.register::<Dog, dyn Mammal>();
        registry.register::<Cat, dyn Animal>();

        assert!(registry.implements::<Dog, dyn Animal>());
        assert!(registry.implements::<Dog, dyn Mammal>());
        assert!(registry.implements::<Cat, dyn Animal>());
        assert!(!registry.implements::<Cat, dyn Mammal>());
    }

    #[test]
    fn test_evidence() {
        let registry = TraitRegistry::new();
        registry.register::<Dog, dyn Animal>();

        let evidence = registry.get_evidence::<Dog, dyn Animal>();
        assert!(evidence.is_some());

        let evidence = registry.get_evidence::<Cat, dyn Animal>();
        assert!(evidence.is_none());
    }

    #[test]
    fn test_query_traits() {
        let registry = TraitRegistry::new();
        registry.register::<Dog, dyn Animal>();
        registry.register::<Dog, dyn Mammal>();

        let traits = registry.traits_of(TypeId::of::<Dog>());
        assert_eq!(traits.len(), 2);
    }
}
