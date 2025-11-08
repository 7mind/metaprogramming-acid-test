# Challenge: A Metaprogramming Acid Test

This test evaluates a language's metaprogramming capabilities by examining whether advanced runtime introspection and code manipulation can be implemented as libraries, without requiring compiler plugins or built-in language features.

## 1. Effortless Structured Logging

Can the language implement a structured logger as a library, without compiler plugins or macros? 

When we write `logger.log(s"Hello ${user}, your balance is ${balance}")`, the program should automatically extract both the template and variable values, printing JSON like:

```json
{
  "template": "Hello %user%, your balance is %balance%", 
  "args": {
    "user": "John", 
    "balance": 42
  }
}
```

## 2. Library-Based Reflection

Can we implement reflection as a library without relying on native language capabilities? 

Our reflection library should support:
- `TypeId[T]` for runtime type identity comparison
- `IsSubtype[A, B]` for subtype checking (in languages with subtyping)

## 3. Functoid Concept

Can the language implement a concept called Functoid? 

A Functoid turns an arbitrary function into a runtime-introspectable entity that can:
- Invoke the original function with dynamically-provided arguments
- Retrieve type tags for each parameter

### 3.1. Named Type Tags

Can we attach custom identifiers to each parameter's type tag?

**Rust Example:**
```rust
#[functoid]
fn greet(#[id("user-name")] name: String, #[id("msg")] msg: String) -> String {
    format!("{}, {}", msg, name)
}
```

**Scala Example:**
```scala
val funcFunctoid: Functoid[Int => String] = Functoid {
  (prefix: String @Id("prefix")) =>
    (n: Int) => s"$prefix-$n"
}
```

**Kotlin Example:**
```kotlin
fun createService(
    @Id("primary") db: Database,
    @Id("cache") cache: Cache
): Service {
    return Service(db, cache)
}

val functoid = FunctoidFactory.fromFunction(::createService)
// Automatically extracts type tags with @Id annotations
functoid.getParameterTypes().forEach { tag ->
    println("${tag.type} @Id(\"${tag.id}\")")
}
```

---

## Language Implementations

| Language | Structured Logging | Reflection | Functoid |
|----------|-------------------|------------|----------|
| **Scala** | ✅ [LogStage](https://github.com/7mind/izumi) | ✅ [izumi-reflect](https://github.com/zio/izumi-reflect) | ✅ [distage](https://github.com/7mind/izumi) |
| **Kotlin** | ❌ | ⚠️ [kotlin-reflect](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.reflect/) | ✅ [kotlin/functoid](kotlin/functoid) |

### Kotlin Implementation Notes

**Functoid (✅)**: Full implementation with:
- Runtime parameter introspection via reified type parameters and `KType`
- Support for functions with 0-5 parameters through typed factory methods
- `@Id("name")` annotation for parameter identification (matching Scala's `@Id`)
- Automatic `@Id` extraction from `KFunction` references via reflection
- Full generic type preservation including nested generics (e.g., `Map<String, List<Int>>`)
- Applicative functor operations: `map`, `map2`, `zip`, `pure`
- Zero third-party dependencies (only `kotlin-reflect`)
- 31 comprehensive tests covering all functionality

**Reflection (⚠️)**: Partial implementation:
- Uses Kotlin's built-in `kotlin-reflect` for runtime type information
- Provides `KType` for type identity comparison and generic type preservation
- Reflection is a native language capability rather than a pure library
- See [kotlin/functoid](kotlin/functoid) for details

**Structured Logging (❌)**: Not yet implemented
- Kotlin string templates don't preserve variable names after compilation
- Would require compiler plugin or inline function with reified context receivers
