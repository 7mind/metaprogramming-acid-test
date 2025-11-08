# Functoid - Runtime-Introspectable Functions for Rust

A Rust port of the Functoid concept from Izumi Distage (Scala), providing runtime-introspectable function wrappers for dependency injection and similar use cases.

## Features

- **Runtime Introspection**: Convert any function into a runtime-introspectable representation
- **Type Information**: Extract parameter types and return types at runtime
- **Invocation**: Call functions with boxed arguments
- **@Id Annotations**: Support for identifier annotations on parameters (similar to Scala's @Id)
- **Zero Dependencies**: No third-party dependencies in the core implementation

## Example

```rust
use functoid::{functoid, Functoid};

// Simple function
#[functoid]
fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Function with @Id annotations
#[functoid]
fn greet(
    #[id("user-name")] name: String,
    #[id("greeting-msg")] msg: String
) -> String {
    format!("{}, {}", msg, name)
}

fn main() {
    // Create a functoid instance
    let add_functoid = AddFunctoid::new();

    // Introspect parameters
    for (i, param) in add_functoid.param_info().iter().enumerate() {
        println!("Param {}: {} (type: {})", i, param.name, param.type_info);
    }

    // Get return type
    println!("Returns: {}", add_functoid.return_type());

    // Invoke with boxed arguments
    let result = add_functoid.invoke(vec![
        Box::new(10i32),
        Box::new(20i32),
    ]);
    println!("Result: {}", result); // 30

    // Original function still works
    assert_eq!(add(10, 20), 30);

    // With @Id annotations
    let greet_functoid = GreetFunctoid::new();
    let params = greet_functoid.param_info();
    assert_eq!(params[0].id, Some("user-name"));
    assert_eq!(params[1].id, Some("greeting-msg"));
}
```

## How It Works

The `#[functoid]` macro:
1. Keeps the original function intact for direct calls
2. Generates a companion struct (e.g., `AddFunctoid` for function `add`)
3. Implements the `Functoid` trait with:
   - `param_info()`: Returns parameter metadata (name, type, optional id)
   - `return_type()`: Returns type information for the return value
   - `invoke()`: Calls the function with runtime-provided boxed arguments

## API

### Core Types

- **`TypeInfo`**: Holds type metadata using `TypeId` and type name
- **`ParamInfo`**: Parameter metadata including name, type, and optional identifier
- **`Functoid` trait**: The main trait for introspectable functions

### Macro

- **`#[functoid]`**: Attribute macro to convert functions into functoids
- **`#[id("identifier")]`**: Parameter attribute for adding identifiers (like Scala's @Id)

## Project Structure

```
rust-functoid/
├── functoid/          # Core library
│   ├── src/
│   │   └── lib.rs    # Trait definitions and core types
│   └── tests/        # Integration tests
├── functoid-macro/    # Procedural macro
│   └── src/
│       └── lib.rs    # Macro implementation
├── flake.nix         # Nix flake for development environment
└── .envrc            # direnv configuration
```

## Development

This project uses Nix flakes and direnv for dependency management:

```bash
# Allow direnv (first time only)
direnv allow

# Build
cargo build

# Test
cargo test

# Run specific test
cargo test test_simple_functoid
```

## Testing

The project includes comprehensive tests covering:
- Functions with no parameters
- Functions with simple parameters
- Functions with different types
- Functions with @Id annotations
- Mixed id and non-id parameters
- Complex return types
- Error cases (wrong argument count, wrong types)
- Type information equality
- Display/Debug implementations

All tests pass:
```bash
cargo test
```

## Limitations

- Only supports simple parameter patterns (no destructuring)
- Parameters must implement `Clone` for invocation
- No support for `self` parameters (trait methods)
- Types must be `'static` for runtime introspection

## Comparison with Izumi Distage

This implementation provides similar functionality to Scala's Functoid:

**Scala (Distage):**
```scala
@Id("user-name") name: String
```

**Rust (this crate):**
```rust
#[id("user-name")] name: String
```

Both allow for:
- Runtime parameter introspection
- Type-safe invocation
- Identifier annotations for disambiguation

## License

This is a demonstration project. Use at your own discretion.
