# Metaprogramming Acid Test - Zig Implementation

This directory contains Zig implementations of all three metaprogramming challenges.

## Overview

Zig's compile-time (`comptime`) capabilities enable powerful metaprogramming without requiring compiler plugins or runtime reflection. All three challenges are implemented as pure library code.

## Implementations

### 1. Effortless Structured Logging ✅

Located in `src/structured_logging.zig`

Zig's comptime string manipulation allows automatic template transformation and named argument extraction using struct-based syntax.

**Example:**
```zig
try logNamed(allocator, writer, "Hello {s}, balance: {d}", .{
    .user = "John",
    .balance = 42,
});
```

**Output:**
```json
{
  "template": "Hello %s%, your balance is %d%",
  "args": {
    "user": "John",
    "balance": 42
  }
}
```

**Features:**
- Compile-time template parsing and transformation
- Named arguments via anonymous struct syntax
- Type-safe value conversion to JSON
- Zero runtime overhead for template processing

**Robust variant:** ✅ Handled by type-safe value conversion with fallback

### 2. Library-Based Reflection ✅

Located in `src/type_reflection.zig`

Zig's `@typeName()` and `@typeInfo()` provide comprehensive type introspection at compile time.

**Example:**
```zig
const id1 = TypeId(i32).init();
const id2 = TypeId(i32).init();
const id3 = TypeId(u32).init();

std.debug.assert(isSame(i32, i32, id1, id2));  // true
std.debug.assert(!isSame(i32, u32, id1, id3)); // false
```

**Features:**
- `TypeId(T)` with compile-time type name and hash
- `isSame(A, B)` for runtime type comparison
- `isSubtypeOf(A, B)` for subtype checking (integer widening, pointer compatibility, etc.)
- `TypeMetadata(T)` for additional type information

**Implementation notes:**
- Uses `@typeName()` for compile-time type names (w/ RT) ✅
- Hash computed at compile time using FNV-1a
- Pure library implementation without compiler intrinsics
- Subtype checking supports integer widening, float widening, and optional wrapping

### 3. Functoid Concept ✅

Located in `src/functoid.zig`

Zig's `@typeInfo()` enables complete function signature introspection at compile time.

**Example:**
```zig
fn greet(name: []const u8, age: i32) bool {
    return age >= 18;
}

const functoid = Functoid(@TypeOf(greet)).init(greet);

// Introspection
std.debug.print("Arity: {}\n", .{functoid.arity()});
const params = try functoid.parameterTypes(allocator);
std.debug.print("Return type: {s}\n", .{functoid.returnType()});

// Invocation
const result = functoid.invoke(.{ "Alice", 25 });
```

**Features:**
- Full parameter type introspection
- Return type introspection
- Runtime function invocation with type checking
- Parameter count (arity) introspection

**Named Type Tags:** ⚠️ Partial

Zig doesn't support annotations like Rust's `#[id("name")]`, but we provide two approaches:

1. **Runtime parameter IDs:**
```zig
const param_ids = &[_]?[]const u8{ "user-name", "user-age" };
const functoid = Functoid(@TypeOf(greet)).initWithIds(greet, param_ids);
```

2. **Compile-time parameter IDs:**
```zig
const FunctoidType = FunctoidWithIds(@TypeOf(greet), &[_][]const u8{
    "user-name",
    "user-age",
});
const functoid = FunctoidType.init(greet);
```

## Building and Testing

### Prerequisites

- Zig 0.13.0 or later

### Build

```bash
cd zig/metaprogramming-challenges
zig build
```

### Run Tests

```bash
zig build test
```

### Run Examples

```bash
zig build run-examples
```

Or run individual examples:

```bash
zig build-exe examples/structured_logging_example.zig -lc
./structured_logging_example

zig build-exe examples/type_reflection_example.zig -lc
./type_reflection_example

zig build-exe examples/functoid_example.zig -lc
./functoid_example
```

## Project Structure

```
zig/metaprogramming-challenges/
├── build.zig                           # Build configuration
├── src/
│   ├── main.zig                        # Main module exports
│   ├── structured_logging.zig          # Challenge 1
│   ├── type_reflection.zig             # Challenge 2
│   └── functoid.zig                    # Challenge 3
├── examples/
│   ├── structured_logging_example.zig
│   ├── type_reflection_example.zig
│   └── functoid_example.zig
└── README.md
```

## Zig's Metaprogramming Capabilities

Zig achieves these metaprogramming features through:

1. **Comptime execution**: Code can run at compile time with `comptime` keyword
2. **Type introspection**: `@typeInfo()` provides full type structure access
3. **Type names**: `@typeName()` gives string representation of types
4. **Generic functions**: Functions can be generic over types
5. **Inline loops**: `inline for` unrolls loops at compile time
6. **Compile-time known values**: Constants and types are first-class comptime values

All three challenges are implementable as pure library code without:
- Compiler plugins
- External code generation tools
- Runtime reflection overhead
- Preprocessor macros (in the C/C++ sense)

## Comparison to Other Languages

| Feature | Zig Approach |
|---------|--------------|
| **Structured Logging** | Comptime string parsing + struct field names |
| **Type Identity** | `@typeName()` + comptime hashing |
| **Subtype Checking** | `@typeInfo()` + comptime type analysis |
| **Functoid** | `@typeInfo(Fn)` for complete signature introspection |
| **Parameter IDs** | Struct-based or compile-time array approach |

Zig's compile-time execution model provides a unique approach: the boundary between compile-time and runtime is explicit and controllable, allowing metaprogramming to happen entirely at compile time while producing zero-overhead runtime code.
