# C++ Metaprogramming Challenges - Complete Implementation

This directory contains pure library implementations of all three metaprogramming challenges in C++20, demonstrating advanced compile-time and runtime metaprogramming capabilities.

## Overview

### Challenge 1: Effortless Structured Logging ✅
### Challenge 2: Library-Based Reflection ✅
### Challenge 3: Functoid Concept ✅

All three challenges are implemented using **library-level metaprogramming** without compiler plugins or language modifications.

---

## Challenge 1: Structured Logging

**Implementation**: Macro-based template extraction with variadic arguments

When you write:
```cpp
std::string user = "John";
int balance = 42;
LOG("Hello {user}, your balance is {balance}", user, balance);
```

Output:
```json
{
  "template": "Hello %user%, your balance is %balance%",
  "args": {
    "user": "John",
    "balance": 42
  }
}
```

### How It Works

Uses C++ preprocessor macros (`__VA_ARGS__`) combined with variadic templates:
- **Macro expansion**: Captures variable names using `#var` stringification
- **Template processing**: Replaces `{var}` with `%var%` at compile time
- **Value serialization**: Converts values to JSON-compatible strings

### Robust Logging (Challenge 1.2) ✅

The implementation also includes `LOG_ROBUST` for handling complex expressions safely:

```cpp
LOG_ROBUST("Computed: %result%", "result", compute_value());
LOG_ROBUST("Sum: %sum%, Product: %product%",
           "sum", x + y,
           "product", x * y);
LOG_ROBUST("Length: %len%, substr: %sub%",
           "len", text.length(),
           "sub", text.substr(0, 3));
```

**Key features**:
- Accepts arbitrary expressions, not just variable names
- Uses preprocessor stringification (`#expr`) to capture expression text
- Wraps serialization in try-catch for error handling
- Falls back to `"<expr: expression_text>"` if serialization fails
- Explicit name-to-expression mapping for clarity

**Implementation technique**:
```cpp
#define LOG_ROBUST_PAIR(name, expr) name, #expr, expr

// Helper with try-catch wrapper
std::string build_args_json_robust(std::string_view name,
                                   std::string_view expr_str,
                                   const T& value) {
    try {
        return "\"" + name + "\": " + to_string_repr(value);
    } catch (...) {
        return "\"" + name + "\": \"<expr: " + expr_str + ">\"";
    }
}
```

**Key files**: `include/structured_logging.hpp`, `examples/robust_logging_example.cpp`

---

## Challenge 2: Type Reflection

**Implementation**:
- **TypeId**: Constexpr extraction from `__PRETTY_FUNCTION__` / `__FUNCSIG__`
- **Subtyping**: Compile-time traits + RTTI

### TypeId - Compile-Time Type Identity

```cpp
auto int_id = type_id<int>();
auto string_id = type_id<std::string>();

std::cout << TypeId<int>::name();  // "int"
std::cout << TypeId<int>::hash();  // Compile-time hash

// Runtime type comparison
bool same = is_same(int_id, string_id);  // false
```

### Subtyping Check

```cpp
class Animal { virtual ~Animal() = default; };
class Dog : public Animal {};

// Compile-time check
static_assert(is_subtype_of_compiletime<Dog, Animal>());

// Runtime check
bool is_sub = is_subtype_of(type_id<Dog>(), type_id<Animal>());  // true

// Dynamic RTTI check
Animal* ptr = new Dog();
bool is_dog = is_dynamic_subtype<Animal, Dog>(ptr);  // true
```

### How It Works

**TypeId**:
1. Uses compiler-specific macros to extract type information
   - GCC/Clang: `__PRETTY_FUNCTION__`
   - MSVC: `__FUNCSIG__`
2. Parses function signature at compile time using `constexpr`
3. Computes FNV-1a hash for efficient comparison

**Subtyping**:
- Compile-time: `std::is_base_of_v<Parent, Child>`
- Runtime: RTTI with `dynamic_cast` and `typeid`

**Key files**: `include/type_id.hpp`, `include/subtyping.hpp`

---

## Challenge 3: Functoid

**Implementation**: Template metaprogramming with optional parameter IDs

A Functoid wraps functions with runtime introspection capabilities.

### Basic Usage

```cpp
int add(int x, int y) { return x + y; }

auto functoid = make_simple_functoid(add);

std::cout << functoid.arity();              // 2
std::cout << functoid.return_type_name();   // "int"

auto params = functoid.param_info();
// params[0].type_name == "int"
// params[1].type_name == "int"

int result = functoid.invoke(10, 20);       // 30
```

### With Parameter IDs

```cpp
PARAM_ID(user_name_id, "user-name");
PARAM_ID(msg_id, "greeting-msg");

std::string greet(const std::string& msg, const std::string& name) {
    return msg + ", " + name;
}

auto functoid = make_functoid<
    Parameter<std::string, msg_id>,
    Parameter<std::string, user_name_id>
>(greet);

auto params = functoid.param_info();
// params[0].id == "greeting-msg"
// params[1].id == "user-name"
```

### How It Works

1. **Function traits extraction**: Template metaprogramming to decompose function signatures
2. **Type erasure**: Base class interface for polymorphic storage
3. **Parameter metadata**: Template parameters encode both type and optional ID
4. **ID attachment**: Constexpr string literals attached via template parameters

**Limitations**:
- C++ doesn't support true annotations like Rust's `#[id("name")]`
- IDs must be defined as constexpr variables using `PARAM_ID` macro
- Slightly more verbose than annotation-based approaches

**Key files**: `include/functoid.hpp`

---

## Building and Testing

### With Nix (Recommended)

```bash
# Enter development environment
nix develop

# Build
cmake -B build -G Ninja
cmake --build build

# Run tests
ctest --test-dir build --output-on-failure

# Run examples
./build/example_all
./build/example_structured_logging
./build/example_robust_logging
./build/example_type_reflection
./build/example_functoid
```

### Without Nix

Requirements:
- C++20 compiler (GCC 10+, Clang 11+, MSVC 2019+)
- CMake 3.20+

```bash
mkdir build && cd build
cmake ..
cmake --build .
ctest --output-on-failure
```

---

## Project Structure

```
cpp/metaprogramming-challenges/
├── include/
│   ├── structured_logging.hpp   # Challenge 1
│   ├── type_id.hpp              # Challenge 2 - TypeId
│   ├── subtyping.hpp            # Challenge 2 - Subtyping
│   └── functoid.hpp             # Challenge 3
├── examples/
│   ├── structured_logging_example.cpp
│   ├── robust_logging_example.cpp
│   ├── type_reflection_example.cpp
│   ├── functoid_example.cpp
│   └── all_examples.cpp
├── tests/
│   ├── test_structured_logging.cpp
│   ├── test_type_reflection.cpp
│   └── test_functoid.cpp
├── CMakeLists.txt
├── flake.nix
└── README.md
```

---

## Implementation Techniques

### Compile-Time Metaprogramming

1. **Template Metaprogramming**: Function traits extraction, type decomposition
2. **Constexpr Evaluation**: Compile-time string parsing and hashing
3. **SFINAE/Concepts**: Type constraints and trait checking
4. **Variadic Templates**: Handling arbitrary parameter counts

### Preprocessor Metaprogramming

1. **Stringification**: `#var` → `"var"`
2. **Token Pasting**: Macro argument manipulation
3. **Variadic Macros**: `__VA_ARGS__` for flexible argument handling

### Runtime Techniques

1. **RTTI**: `typeid`, `dynamic_cast` for runtime type checking
2. **Type Erasure**: Virtual interfaces for polymorphic storage
3. **Function Traits**: Extracting signatures from lambdas and functors

---

## Compiler Support

| Compiler | Version | Support |
|----------|---------|---------|
| GCC      | 10+     | ✅ Full |
| Clang    | 11+     | ✅ Full |
| MSVC     | 2019+   | ✅ Full |

All implementations use C++20 features:
- Concepts
- constexpr enhancements
- Template improvements
- std::string_view in constexpr contexts

---

## Comparison with Other Languages

### Challenge 1: Structured Logging

**C++ Approach**:
- Preprocessor macros for variable name capture
- Compile-time template string processing
- Zero runtime parsing overhead

**vs Python**: Runtime frame introspection (more dynamic, runtime overhead)
**vs Rust**: Declarative macros (similar compile-time approach)

### Challenge 2: Type Reflection

**C++ Approach**:
- Compiler intrinsics for type names
- Compile-time hash computation
- RTTI for dynamic checks

**Advantages**:
- Pure library solution (no external tools)
- Compile-time type information
- Strong type safety

**Limitations**:
- Name demangling varies by compiler
- RTTI overhead for polymorphic types

### Challenge 3: Functoid

**C++ Approach**:
- Template metaprogramming for function decomposition
- Constexpr strings for IDs (via macro)
- Template parameters encode metadata

**vs Rust**: Procedural macros allow true annotations
**vs Scala**: Compiler reflection provides richer metadata

**C++ Achievement**: Demonstrates that sophisticated introspection is possible through template metaprogramming alone.

---

## Key Insights

1. **C++ is surprisingly capable** at library-level metaprogramming through:
   - Template metaprogramming
   - Constexpr evaluation
   - Preprocessor macros
   - Compiler intrinsics

2. **No compiler plugins needed** - everything is achieved through:
   - Standard C++20 features
   - Compiler-specific but standardized intrinsics
   - Creative template usage

3. **Trade-offs**:
   - More verbose than annotation-based systems
   - Compile-time overhead vs runtime flexibility
   - Macro complexity for some patterns

---

## Challenge Status

| Challenge | Status | Implementation |
|-----------|--------|----------------|
| 1. Structured Logging | ✅ | Macro + template based |
| 1.2. Robust Logging | ✅ | Preprocessor stringification + try-catch |
| 2. Type Reflection | ✅ | Compiler intrinsics + RTTI |
| 3. Functoid | ✅ | Template metaprogramming + constexpr IDs |

All challenges successfully implemented as pure library solutions!
