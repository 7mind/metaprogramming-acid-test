# Functoid Kotlin

A Kotlin port of the Functoid concept from [Izumi Distage](https://izumi.7mind.io/distage/), providing runtime-introspectable function representations for dependency injection and type-safe programming.

## Features

- **Runtime Type Introspection**: Get type information about function parameters at runtime
- **@Id Annotation Support**: Mark dependencies with identifiers for named injection
- **Type-Safe**: Full compile-time type safety with reified generics
- **Applicative Functor**: Compose Functoids using `map`, `map2`, and `zip` operations
- **Zero Third-Party Dependencies**: Pure Kotlin implementation using only kotlin-reflect
- **Comprehensive Tests**: Well-tested implementation with high coverage

## What is a Functoid?

A Functoid is a wrapper around a function that preserves runtime type information and allows introspection of:
- Parameter types
- Return type
- Function arity
- Parameter identifiers (@Id annotations)

This is particularly useful for:
- Dependency injection frameworks
- Configuration systems
- Dynamic function dispatch
- Type-safe metaprogramming

## Quick Start

### Setup

The project uses Nix flakes and direnv for dependency management:

```bash
# Allow direnv
direnv allow

# Build the project
gradle build

# Run tests
gradle test
```

### Basic Usage

```kotlin
import io.functoid.*

// Create a simple Functoid
val functoid = FunctoidFactory.from<Int, Int>({ x -> x * 2 })

// Introspect the function
println("Arity: ${functoid.getArity()}")  // 1
println("Parameter types: ${functoid.getParameterTypes()}")  // [kotlin.Int]
println("Return type: ${functoid.getReturnType()}")  // kotlin.Int

// Invoke the function
val result = functoid.invoke(listOf(21))  // 42
```

### Using @Id Annotations

```kotlin
import io.functoid.*

// Create a service function with named dependencies
fun createService(
    @Id("primary") db: Database,
    @Id("cache") cache: Cache
): Service {
    return Service(db, cache)
}

// Create Functoid from function reference
val functoid = FunctoidFactory.fromFunction(::createService)

// Inspect parameter identifiers
functoid.getParameterTypes().forEach { typeTag ->
    println("Type: ${typeTag.type}, Id: ${typeTag.id}")
}
// Output:
// Type: Database, Id: primary
// Type: Cache, Id: cache
```

### Composing Functoids

Functoids form an applicative functor, allowing composition:

```kotlin
// Create two functoids
val f1 = FunctoidFactory.from<Int, Int>({ x -> x + 1 })
val f2 = FunctoidFactory.from<Int, Int>({ y -> y * 2 })

// Combine using map2
val combined = f1.map2(f2, { a, b -> a + b }, typeOf<Int>())

// Invoke: (5+1) + (10*2) = 26
val result = combined.invoke(listOf(5, 10))
```

### Mapping Over Results

```kotlin
val functoid = FunctoidFactory.from<Int, Int>({ x -> x * 2 })
val mapped = functoid.map({ it + 1 }, typeOf<Int>())

mapped.invoke(listOf(5))  // (5 * 2) + 1 = 11
```

## API Reference

### FunctoidFactory

Factory methods for creating Functoids from functions:

```kotlin
// Zero parameters
FunctoidFactory.from { 42 }

// One parameter
FunctoidFactory.from<Int, Int>({ x -> x * 2 })

// Two parameters with @Id annotations
FunctoidFactory.from<Int, String, String>(
    { x, y -> "$y: $x" },
    id1 = "number",
    id2 = "label"
)

// From function reference (automatic @Id extraction)
FunctoidFactory.fromFunction(::myFunction)
```

Supports functions with 0-5 parameters. For more parameters, use `fromFunction()`.

### Functoid Methods

```kotlin
class Functoid<A> {
    // Introspection
    fun getArity(): Int
    fun getParameterTypes(): List<TypeTag<*>>
    fun getReturnType(): KType

    // Invocation
    fun invoke(args: List<Any?>): Any?

    // Applicative operations
    fun <B> map(f: (A) -> B, returnType: KType): Functoid<B>
    fun <B, C> map2(that: Functoid<B>, f: (A, B) -> C, returnType: KType): Functoid<C>
    fun <B> zip(that: Functoid<B>): Functoid<Pair<A, B>>

    companion object {
        fun <A> pure(value: A, returnType: KType): Functoid<A>
    }
}
```

### TypeTag

Runtime type information holder:

```kotlin
data class TypeTag<T>(
    val type: KType,
    val id: String? = null
) {
    fun hasId(): Boolean

    companion object {
        inline fun <reified T> of(id: String? = null): TypeTag<T>
        fun from(type: KType, id: String? = null): TypeTag<*>
    }
}
```

### @Id Annotation

Mark parameters with identifiers:

```kotlin
@Target(AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.TYPE)
@Retention(AnnotationRetention.RUNTIME)
annotation class Id(val value: String)
```

## Architecture

The implementation consists of four main components:

1. **TypeTag**: Wraps `KType` with optional identifier
2. **@Id**: Annotation for marking named dependencies
3. **Provider**: Internal representation storing function + type info
4. **Functoid**: Public API with applicative functor operations
5. **FunctoidFactory**: Factory methods for creating Functoids

## Design Principles

Following the user's guidelines:

- ✅ **Fail Fast**: Uses `require()` for argument validation
- ✅ **Explicit over Implicit**: No default parameters in core APIs
- ✅ **Type Safety**: Uses reified generics and `KType`
- ✅ **SOLID**: Clear separation of concerns
- ✅ **No Third-Party Libraries**: Only kotlin-reflect
- ✅ **No Magic Constants**: Named constants throughout

## Testing

Comprehensive test suite covering:

- All function arities (0-5 parameters)
- @Id annotation extraction
- Functoid composition (map, map2, zip)
- Type introspection
- Error handling

Run tests:

```bash
gradle test
```

## Comparison with Izumi Distage

| Feature | Izumi Distage | Functoid Kotlin |
|---------|--------------|-----------------|
| Runtime Introspection | ✅ | ✅ |
| @Id Annotations | ✅ | ✅ |
| Applicative Functor | ✅ | ✅ |
| Language | Scala | Kotlin |
| Reflection | Custom TypeTags | kotlin-reflect |
| Max Arity | Arbitrary | 5 (extensible) |

## License

This is a learning/draft project demonstrating the Functoid concept in Kotlin.

## References

- [Izumi Distage Documentation](https://izumi.7mind.io/distage/)
- [Functoid API](https://izumi.7mind.io/api/izumi/distage/model/providers/Functoid.html)
- [The Magnet Pattern](http://spray.io/blog/2012-12-13-the-magnet-pattern/)
