# Challenge: A Metaprogramming Acid Test

This test evaluates a language's metaprogramming capabilities by examining whether advanced runtime introspection and code manipulation can be implemented as libraries, without requiring compiler plugins or built-in language features.

Open your P/Rs. Isolated examples are preferred over links to external libraries. 

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
- `TypeId[T]` with `is_same[A, B](id1: TypeId[A], id2: TypeId[B])` operation for run-time type identity comparison.
- `IsSubtype[A, B]` `is_subtype_of[A, B](child: TypeId[A], parent: TypeId[B])` for run-time subtype checking (in languages with subtyping)

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
| **Rust** | ❓ TODO | ⚠️ [Partial](rust/functoid) | ✅ [rust/functoid](rust/functoid) |
| **Kotlin** | ❌ | ❌ | ✅ [kotlin/functoid](kotlin/functoid) |
| **Python** | ❓ TODO | ❓ TODO | ✅ [Chibi Izumi](https://github.com/7mind/izumi-chibi-py) |
| **Typescript** | ❌ | ⚠️ Can be emulated | ⚠️ [Chibi Izumi](https://github.com/7mind/izumi-chibi-ts) |


## Native Language capabilities

| Language | Type id | Subtype check | Macros/CSE |
|----------|-------------------|------------|----------|
| **Scala** | ⚠️ Scala 2 only | ⚠️ Scala 2 Only | ✅ AST-level |
| **Rust** | ✅ | ❌ | ⚠️ Token-stream level |
| **Kotlin** | ✅ | ✅ | ❌ |
| **Python** | ⚠️ Partial | ⚠️ Partial | ⚠️ Can be emulated |
| **TypeScript** | ❌ | ❌ | ❌ |

