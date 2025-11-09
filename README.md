# Metaprogramming Acid Test Challenge

[![Test Implementations](https://github.com/7mind/metaprogramming-acid-test/actions/workflows/test.yml/badge.svg)](https://github.com/7mind/metaprogramming-acid-test/actions/workflows/test.yml)

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

A perfect implementation should not fail if it meets a function argument of unexpected shape (e.g. a call to another function).

## 2. Library-Based Reflection

Can we implement reflection as a library without relying on native language capabilities?

Our reflection library should support:
- `TypeId[T]` with `is_same[A, B](id1: TypeId[A], id2: TypeId[B])` operation for run-time type identity comparison.
- Operation `is_subtype_of[A, B](child: TypeId[A], parent: TypeId[B])` for run-time subtype checking (in languages with subtyping)

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

## Summary

We would like to see if the following can be done:

1. Structural logger with automatic argument name extraction
2. Robust structural logger with automatic argument name extraction which won't fail on unexpected argument shapes (e.g. a function call or some expression passed as an argument)
3. Run-time comparable `TypeId[T]` using compiler intrinsics, standard library or runtime features
4. Run-time comparable `TypeId[T]` implementable fully as a library (pure)
5. Run-time `is_subtype(TypeId[A], TypeId[B])` operation using compiler intrinsics, standard library or runtime features
6. Run-time `is_subtype(TypeId[A], TypeId[B])` operation implementable fully as a library (pure)
7. `Functoid` implementation
8. `Functoid` implementation which supports named arguments

---

## Implementations

| Language | 1. Logger | 2. Logger (robust) | 3. TypeId (w/ RT) | 4. TypeId (pure) | 5. Subtype (w/ RT) | 6. Subtype (pure) | 7. Functoid | 8. Functoid (@Id) |
|----------|-----------|-------------------|-------------------|------------------|-------------------|-------------------|-------------|-------------------|
| **Scala** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Python** | [✅](python/structured_logging) | [✅](python/structured_logging) | ✅ | ➖ | ✅ | ➖ | ✅ | ✅ |
| **C++** | [✅](cpp/metaprogramming-challenges) | [✅](cpp/metaprogramming-challenges) | [✅](cpp/metaprogramming-challenges) | [⚠️](cpp/metaprogramming-challenges) | [✅](cpp/metaprogramming-challenges) | [⚠️](cpp/metaprogramming-challenges) | [✅](cpp/metaprogramming-challenges) | ❌ |
| **Rust** | [✅](rust/structured_logging) | [✅](rust/structured_logging) | [✅](rust/functoid) | ❌ | ❌ | ❌ | [✅](rust/functoid) | [✅](rust/functoid) |
| **Haskell** | [✅](haskell/metaprogramming-challenges) | ❓ | [✅](haskell/metaprogramming-challenges) | ❓ | ➖ | ➖ | [✅](haskell/metaprogramming-challenges) | ⚠️ |
| **Zig** | [✅](zig/metaprogramming-challenges) | [✅](zig/metaprogramming-challenges) | [✅](zig/metaprogramming-challenges) | ❌ | ➖ | ➖ | [✅](zig/metaprogramming-challenges) | ⚠️ |
| **Swift** | [✅](swift/metaprogramming-challenges) | [⚠️](swift/metaprogramming-challenges) | [✅](swift/metaprogramming-challenges) | [⚠️](swift/metaprogramming-challenges) | [⚠️](swift/metaprogramming-challenges) | ❌ | [✅](swift/metaprogramming-challenges) | [⚠️](swift/metaprogramming-challenges) |
| **Kotlin** | ❌ | ❌ | ✅ | ❌ | ✅ | ❌ | [✅](kotlin/functoid) | [✅](kotlin/functoid) |
| **TypeScript** | ❌ | ❌ | ➖ | ⚠️ | ➖ | ❌ | ⚠️ | ⚠️ |

**Legend:**
- ✅ - Complete working implementation
- ⚠️ - Partial implementation (relies on conventions, has limitations, or uses workarounds)
- ❓ - Implementation possible and pending
- ➖ - Not applicable
- ❌ - Likely impossible to implement
- Links point to implementation directories

**Notes:**
- **Scala**: Reference implementation (all features via izumi ecosystem)
- **Kotlin**: Native reflection for TypeId and subtyping; Functoid with @Id annotations
- **Python**: Frame introspection for logging; runtime reflection TypeId via type(); `@id` through `Annotated`
- **Rust**: Macros for logging; TypeId via std::any::TypeId; Functoid with #[id] attrs
- **TypeScript**: Convention-based Functoid with compile-time metadata enforcement
- **Haskell**: Template Haskell for basic variable capture (robust logger impossible - would require parsing Haskell syntax from strings); Typeable/Type.Reflection for runtime type identity; GADTs with type-level strings for functoids (parameter IDs via type-level strings, not annotations).
- **C++**: Structured logging includes robust variant (`LOG_ROBUST`) that handles arbitrary expressions using preprocessor stringification with try-catch fallback. All three challenges implemented using C++20 template metaprogramming, constexpr evaluation, and compiler intrinsics.
- **Zig**: Comptime execution for all challenges; `@typeName()` for pure library TypeId; `@typeInfo()` for complete function introspection; struct-based named parameters for logging (requires explicit field names like `.{ .user = user, .balance = balance }` as Zig doesn't preserve variable names in tuple arguments); parameter IDs via compile-time arrays or runtime assignment.
- **Swift**: Mirror API for runtime reflection; KeyValuePairs or struct-based logging; TypeId via type name comparison; subtype checking only on macOS/iOS via ObjectiveC runtime; Functoid with manual parameter names (closures don't preserve parameter names at runtime).

### External Library Implementations

**Scala (izumi ecosystem):**
- Structured Logging: [LogStage](https://github.com/7mind/izumi)
- Type Reflection: [izumi-reflect](https://github.com/zio/izumi-reflect)
- Functoid: [distage](https://github.com/7mind/izumi)

**Python:**
- Functoid: [Chibi Izumi](https://github.com/7mind/izumi-chibi-py)

**TypeScript:**
- Functoid: [Chibi Izumi](https://github.com/7mind/izumi-chibi-ts) (partial)

## Native Language capabilities

### Reflection

| Language | Run-time Type Id | Run-time Subtype check |
|----------|-------------------|----------------------|
| **Scala** | ❌⚠️ [Scala 2 only](https://www.scala-lang.org/api/2.13.x/scala-reflect/scala/reflect/api/TypeTags.html) | ❌⚠️ Scala 2 Only |
| **Rust** | ✅ [TypeId](https://doc.rust-lang.org/std/any/struct.TypeId.html) | ❌ |
| **Haskell** | ✅ [Typeable](https://hackage.haskell.org/package/base/docs/Data-Typeable.html) | ➖ |
| **Kotlin** | ✅ [KClass](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.reflect/-k-class/) | ✅ [Reflection](https://kotlinlang.org/docs/reflection.html) |
| **Python** | ⚠️ [type()](https://docs.python.org/3/library/functions.html#type) | ⚠️ [issubclass()](https://docs.python.org/3/library/functions.html#issubclass) |
| **TypeScript** | ❌ | ❌ |
| **C++** | ⚠️ [typeid](https://en.cppreference.com/w/cpp/language/typeid) | ⚠️ [SFINAE](https://en.cppreference.com/w/cpp/language/sfinae) |
| **Zig** | ✅ [@typeInfo](https://ziglang.org/documentation/master/#typeInfo) | ❌ |
| **Swift** | ✅ [Mirror](https://developer.apple.com/documentation/swift/mirror) | ✅ [Type Casting](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/typecasting/) |

### Code Generation

| Language | Macros/CSE |
|----------|------------|
| **Scala** | ✅ [AST-level](https://docs.scala-lang.org/overviews/macros/overview.html) |
| **Rust** | ⚠️ [Token-stream](https://doc.rust-lang.org/reference/procedural-macros.html) |
| **Haskell** | ✅ [Template Haskell](https://wiki.haskell.org/Template_Haskell) |
| **Kotlin** | ❌ |
| **Python** | ⚠️ [inspect](https://docs.python.org/3/library/inspect.html) |
| **TypeScript** | ❌ |
| **C++** | ⚠️ [Templates](https://en.cppreference.com/w/cpp/language/templates) |
| **Zig** | ✅ [comptime](https://ziglang.org/documentation/master/#comptime) |
| **Swift** | ⚠️ [Macros](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/macros/) |

### Inheritance

| Language | Interface w/dynamic dispatch | Interface w/static dispatch |
|----------|----------------------------|---------------------------|
| **Scala** | ✅ [Traits](https://docs.scala-lang.org/tour/traits.html) | ✅ [Inline](https://docs.scala-lang.org/scala3/reference/metaprogramming/inline.html) |
| **Rust** | ✅ [Trait objects](https://doc.rust-lang.org/book/ch17-02-trait-objects.html) | ✅ [Traits](https://doc.rust-lang.org/book/ch10-02-traits.html) |
| **Haskell** | ➖ | ✅ [Typeclasses](https://wiki.haskell.org/Typeclassopedia) |
| **Kotlin** | ✅ [Interfaces](https://kotlinlang.org/docs/interfaces.html) | ⚠️ [Inline](https://kotlinlang.org/docs/inline-functions.html) |
| **Python** | ✅ [ABC/Protocol](https://docs.python.org/3/library/abc.html) | ❌ |
| **TypeScript** | ❌ | ✅ [Interfaces](https://www.typescriptlang.org/docs/handbook/interfaces.html) |
| **C++** | ✅ [Virtual](https://en.cppreference.com/w/cpp/language/virtual) | ✅ [CRTP](https://en.cppreference.com/w/cpp/language/crtp) |
| **Zig** | ⚠️ [Manual vtables](https://ziglang.org/documentation/master/#comptime) | ✅ [comptime](https://ziglang.org/documentation/master/#comptime) |
| **Swift** | ✅ [Protocols](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/protocols/) | ✅ [Witness tables](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/protocols/) |

### Type System

| Language | HKT | Incoherent Typeclasses | Coherent Typeclasses | Implicits | Do-notation |
|----------|-----|----------------------|---------------------|-----------|-------------|
| **Scala** | ✅ [Kind polymorphism](https://docs.scala-lang.org/scala3/reference/other-new-features/kind-polymorphism.html) | ✅ [Incoherent](https://docs.scala-lang.org/scala3/reference/contextual/givens.html) | ⚠️ Requires discipline | ✅ [given/using](https://docs.scala-lang.org/scala3/reference/contextual/givens.html) | [✅](https://docs.scala-lang.org/tour/for-comprehensions.html) |
| **Rust** | ❌ | ❌ [Orphan rules](https://doc.rust-lang.org/book/ch10-02-traits.html#implementing-a-trait-on-a-type) | ✅ [Trait coherence](https://doc.rust-lang.org/book/ch10-02-traits.html#implementing-a-trait-on-a-type) | ➖ | ❌ |
| **Haskell** | ✅ [Kind system](https://wiki.haskell.org/Kind) | ⚠️ [Extension](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#overlapping-instances) | ✅ [Default](https://wiki.haskell.org/Typeclassopedia) | ➖ | [✅](https://wiki.haskell.org/Keywords#do) |
| **Kotlin** | ❌ | ➖ | ➖ | ❌ | ⚠️ [Coroutines](https://kotlinlang.org/docs/coroutines-overview.html) |
| **Python** | ❌ | ➖ | ➖ | ❌ | ⚠️ [async/await](https://docs.python.org/3/library/asyncio-task.html) |
| **TypeScript** | ⚠️ [Workarounds](https://www.matechs.com/blog/encoding-hkts-in-typescript-once-again) | ➖ | ➖ | ❌ | ⚠️ [async/await](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-7.html) |
| **C++** | ⚠️ [Template template](https://en.cppreference.com/w/cpp/language/template_parameters#Template_template_parameter) | ➖ | ➖ | ❌ | ⚠️ [co_await](https://en.cppreference.com/w/cpp/language/coroutines) |
| **Zig** | ⚠️ [comptime](https://ziglang.org/documentation/master/#Generic-Data-Structures) | ➖ | ➖ | ❌ | ❌ |
| **Swift** | ❌ | ➖ | ⚠️ [Protocols](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/protocols/) | ❌ | ⚠️ [async/await](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/) |

