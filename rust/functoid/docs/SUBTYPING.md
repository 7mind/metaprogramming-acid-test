# Runtime Subtyping Evidence in Rust

## The Problem

**Q: Can Rust's `TypeId` check subtyping relationships like Scala's izumi-reflect `<:<` operator?**

**A: No.** `TypeId` only provides exact type equality checking. It cannot tell you:
- If `Dog` implements `Animal`
- If a type satisfies a trait bound
- Any subtyping or trait implementation relationships

### Why Not?

```rust
use std::any::TypeId;

trait Animal { }
struct Dog { }
impl Animal for Dog { }

// These are DIFFERENT TypeIds
let dog_id = TypeId::of::<Dog>();
let animal_id = TypeId::of::<dyn Animal>();

assert_ne!(dog_id, animal_id); // They're not equal!
```

Rust doesn't have JVM-style runtime reflection. The compiler knows `Dog: Animal`, but this information is erased at runtime (except for vtable pointers in trait objects).

## The Solution: Trait Registry

We can build a **runtime trait registry** that provides subtyping-like evidence, similar to izumi-reflect's `<:<` operator.

### Architecture

```
┌─────────────────────────────────────────────┐
│           TraitRegistry                     │
├─────────────────────────────────────────────┤
│  implementations: TypeId -> Set<TraitId>    │
│  implementors: TraitId -> Set<TypeId>       │
└─────────────────────────────────────────────┘
         ↓                           ↓
    Register                      Query
  Dog: Animal                  Dog <: Animal?
  Dog: Mammal                  Fish <: Mammal?
```

### Core Components

#### 1. TraitId - Unique Identifier for Traits

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraitId {
    type_id: TypeId,
    name: &'static str,
}

// Create a TraitId
let animal_trait = TraitId::of::<dyn Animal>();
```

#### 2. ImplEvidence - Proof that T implements Trait

```rust
pub struct ImplEvidence {
    concrete_type: TypeId,
    trait_id: TraitId,
}

// Similar to Scala's <:<[Dog, Animal]
let evidence = ImplEvidence::new::<Dog, dyn Animal>();
```

#### 3. TraitRegistry - Runtime Registry

```rust
pub struct TraitRegistry {
    // Bidirectional mappings
    implementations: HashMap<TypeId, HashSet<TraitId>>,
    implementors: HashMap<TraitId, HashSet<TypeId>>,
}
```

### Usage

#### Step 1: Register Implementations

```rust
use functoid::register_impl;

// Register that Dog implements various traits
register_impl!(Dog: dyn Animal);
register_impl!(Dog: dyn Mammal);
register_impl!(Dog: dyn Pet);

register_impl!(Fish: dyn Animal);
register_impl!(Fish: dyn Pet);
// Note: Fish does NOT implement Mammal
```

#### Step 2: Check Subtyping at Runtime

```rust
use functoid::subtyping::implements;

// Check if Dog implements Animal (like <:<)
assert!(implements::<Dog, dyn Animal>());  // true
assert!(implements::<Dog, dyn Mammal>());  // true

// Fish is not a Mammal
assert!(!implements::<Fish, dyn Mammal>()); // false
```

#### Step 3: Get Explicit Evidence

```rust
use functoid::subtyping::evidence;

// Get evidence (Some if implements, None otherwise)
if let Some(ev) = evidence::<Dog, dyn Mammal>() {
    println!("✓ Evidence: {}", ev);
    // Output: "Dog <: dyn Mammal"
}

match evidence::<Fish, dyn Mammal>() {
    Some(_) => println!("Fish is a Mammal"),
    None => println!("No evidence that Fish is a Mammal"),
}
```

#### Step 4: Query Relationships

```rust
use functoid::subtyping::global_trait_registry;

// Get all traits implemented by Dog
let traits = global_trait_registry().traits_of(TypeId::of::<Dog>());
for trait_id in traits {
    println!("Dog implements: {}", trait_id);
}

// Get all types implementing Mammal
let types = global_trait_registry().implementors_of(TraitId::of::<dyn Mammal>());
println!("Found {} types that implement Mammal", types.len());
```

## Comparison with Scala/izumi-reflect

| Aspect | Scala (izumi-reflect) | Rust (our solution) |
|--------|----------------------|---------------------|
| **Subtyping check** | `implicitly[Dog <:< Animal]` | `implements::<Dog, dyn Animal>()` |
| **Evidence type** | `<:<[Dog, Animal]` | `ImplEvidence` |
| **Registration** | Automatic (JVM reflection) | Manual (explicit registration) |
| **Compile-time safety** | Type-safe implicits | Type-safe generics |
| **Runtime overhead** | Reflection overhead | HashMap lookups |
| **Failure mode** | Compile error if no evidence | Returns `None` at runtime |

### Example Comparison

**Scala:**
```scala
import izumi.reflect.macros.{<:<}

def feedMammal[T: <:<[Mammal]](animal: T): Unit = {
  // Compiler proves T is a subtype of Mammal
}

feedMammal(new Dog)  // ✓ compiles
feedMammal(new Fish) // ✗ compile error
```

**Rust:**
```rust
fn can_feed_as_mammal<T: 'static>() -> bool {
    implements::<T, dyn Mammal>()
}

// Runtime checks
can_feed_as_mammal::<Dog>()  // true
can_feed_as_mammal::<Fish>() // false
```

## Use Cases

### 1. Dependency Injection

```rust
// Check if we can provide a dependency
if implements::<Dog, dyn Animal>() {
    // We can inject Dog where Animal is required
    registry.register_provider(TypeId::of::<dyn Animal>(), dog_factory);
}
```

### 2. Plugin Systems

```rust
// Check if a plugin type implements required interfaces
for plugin_type in available_plugins {
    if type_implements(plugin_type, TraitId::of::<dyn Plugin>()) {
        load_plugin(plugin_type);
    }
}
```

### 3. Runtime Type Resolution

```rust
// Resolve the most specific type for a trait
let implementors = global_trait_registry()
    .implementors_of(TraitId::of::<dyn Service>());

for impl_type in implementors {
    if has_higher_priority(impl_type) {
        return Some(impl_type);
    }
}
```

## Limitations

### 1. Manual Registration Required

Unlike JVM's automatic reflection, you must manually register each implementation:

```rust
// You MUST do this for each impl
register_impl!(Dog: dyn Animal);
```

**Why?** Rust doesn't have runtime reflection. The compiler knows about trait impls, but that information is gone after compilation.

### 2. Runtime Only

These checks happen at runtime, not compile time:

```rust
// This compiles, but returns false at runtime
let result = implements::<Fish, dyn Mammal>(); // false
```

In Scala with `<:<`, this would be a compile error.

### 3. Requires 'static Types

All types must be `'static`:

```rust
// This won't work with non-'static types
// implements::<&'a str, dyn SomeTrait>() // ✗ Error
```

### 4. No Automatic Hierarchy

Parent traits must be registered separately:

```rust
trait Mammal: Animal { }
impl Mammal for Dog { }

// You need BOTH registrations:
register_impl!(Dog: dyn Animal);  // Not automatic!
register_impl!(Dog: dyn Mammal);
```

## Implementation Details

### Thread Safety

The global registry uses `RwLock` for thread-safe access:

```rust
pub struct TraitRegistry {
    implementations: RwLock<HashMap<TypeId, HashSet<TraitId>>>,
    implementors: RwLock<HashMap<TraitId, HashSet<TypeId>>>,
}
```

### Lazy Initialization

The global registry is initialized lazily using `OnceLock`:

```rust
pub fn global_trait_registry() -> &'static Arc<TraitRegistry> {
    use std::sync::OnceLock;
    static REGISTRY: OnceLock<Arc<TraitRegistry>> = OnceLock::new();
    REGISTRY.get_or_init(|| Arc::new(TraitRegistry::new()))
}
```

### Zero Runtime Cost When Not Used

If you don't use the trait registry, it adds no runtime overhead. It's only initialized when first accessed.

## Best Practices

### 1. Register Early

Register implementations at program startup:

```rust
fn register_all_impls() {
    register_impl!(Dog: dyn Animal);
    register_impl!(Dog: dyn Mammal);
    register_impl!(Cat: dyn Animal);
    register_impl!(Cat: dyn Mammal);
    register_impl!(Fish: dyn Animal);
}

fn main() {
    register_all_impls();
    // ... rest of your program
}
```

### 2. Use Const Assertions Where Possible

Prefer compile-time trait bounds when you can:

```rust
// Prefer this (compile-time)
fn feed<T: Animal>(animal: T) { }

// Over this (runtime)
fn feed_dynamic<T: 'static>(animal: T) {
    assert!(implements::<T, dyn Animal>());
}
```

### 3. Cache Evidence

Cache evidence objects when making repeated checks:

```rust
// Cache the evidence
let dog_is_animal = evidence::<Dog, dyn Animal>().unwrap();

// Use it multiple times
for _ in 0..1000 {
    // No need to check again
    use_evidence(&dog_is_animal);
}
```

## Conclusion

While Rust's `TypeId` cannot check subtyping relationships directly, we can build a **runtime trait registry** that provides similar functionality to izumi-reflect's `<:<` operator.

**Trade-offs:**
- ✓ Runtime type checking and resolution
- ✓ Flexible dependency injection
- ✓ Zero dependencies (pure stdlib)
- ✗ Manual registration required
- ✗ Runtime checks instead of compile-time
- ✗ No automatic trait hierarchy

This approach is useful for:
- Dependency injection frameworks
- Plugin systems
- Dynamic type resolution
- Runtime type discovery

But remember: when possible, prefer Rust's compile-time trait bounds for better safety and performance.
