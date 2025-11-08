# Haskell Metaprogramming Challenges - Complete Implementation

This directory contains pure library implementations of all three metaprogramming challenges in Haskell, demonstrating advanced compile-time and runtime metaprogramming capabilities through Template Haskell, Typeable, and the type system.

## Overview

### Challenge 1: Effortless Structured Logging ✅
### Challenge 2: Library-Based Reflection ✅
### Challenge 3: Functoid Concept ✅

All three challenges are implemented using **library-level metaprogramming** without compiler plugins or language modifications.

---

## Challenge 1: Structured Logging

**Implementation**: Template Haskell for compile-time variable capture

When you write:
```haskell
let user = "John"
    balance = 42
in $(logTH "Hello {user}, your balance is {balance}")
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

Uses Template Haskell to capture variable names at compile time:
- **Template parsing**: Extracts `{var}` placeholders and replaces with `%var%`
- **Variable capture**: `logTH` generates code that looks up variables by name
- **JSON serialization**: Uses Aeson to convert values to JSON

**Key features**:
- Automatic variable name extraction
- Type-safe value serialization
- Zero runtime parsing overhead
- Quasi-quoter syntax available: `[logQQ|template|]`

**Key files**: `src/Metaprogramming/StructuredLogging.hs`

---

## Challenge 2: Type Reflection

**Implementation**:
- **TypeId**: Built on `Typeable` and `Type.Reflection`
- **Subtyping**: Modeled via type classes

### TypeId - Runtime Type Identity

```haskell
let intId = typeId @Int
    strId = typeId @String

isSame intId strId  -- False
typeName intId      -- "Int"
typeFingerprint intId  -- Unique fingerprint
```

### Subtyping via Type Classes

```haskell
-- Define subtype relationship
instance SubtypeOf Int Rational where
    upcast = fromIntegral

-- Use it
let val = 42 :: Int
    rational = upcast val :: Rational
```

### How It Works

**TypeId**:
1. Wraps `TypeRep` from `Type.Reflection`
2. Uses `eqTypeRep` for type equality checking
3. Provides `Fingerprint` for hashing

**Subtyping**:
- Modeled as type class `SubtypeOf sub super`
- Provides `upcast :: sub -> super` operation
- Runtime casting via `canCastTo :: a -> TypeId b -> Maybe b`

**Note**: Haskell doesn't have OOP-style subtyping. We model it through type classes, which is more flexible but different from inheritance-based subtyping.

**Key files**: `src/Metaprogramming/TypeReflection.hs`

---

## Challenge 3: Functoid

**Implementation**: GADTs + Typeable + type-level strings for parameter IDs

A Functoid wraps functions with runtime introspection capabilities.

### Basic Usage

```haskell
let greet :: String -> String
    greet name = "Hello, " ++ name

    functoid = functoid1 @"user-name" greet

arity functoid              -- 1
paramTypeNames functoid     -- ["[Char]"]
paramIds functoid           -- [Just "user-name"]
returnTypeName functoid     -- "[Char]"
invoke functoid "Alice"     -- "Hello, Alice"
```

### With Parameter IDs

```haskell
let add :: Int -> Int -> Int
    add x y = x + y

    functoid = functoid2 @"left" @"right" add

paramInfo functoid
-- [ParamInfo {paramType = "Int", paramId = Just "left"},
--  ParamInfo {paramType = "Int", paramId = Just "right"}]

invoke functoid 10 20  -- 30
```

### How It Works

1. **GADTs**: Define different constructors for different arities
2. **Typeable constraints**: Capture type information at runtime
3. **Type-level strings**: Use `KnownSymbol` for parameter IDs via `@"id"` syntax
4. **Smart constructors**: `functoid1`, `functoid2`, `functoid3` for different arities
5. **Type class for invocation**: Polymorphic `invoke` function

**Limitations**:
- Fixed arity (0-3 parameters supported in this implementation)
- Could be extended with more GADT constructors or type-level lists

**Key files**: `src/Metaprogramming/Functoid.hs`

---

## Building and Testing

### With Nix (Recommended)

```bash
# Enter development environment
cd haskell/metaprogramming-challenges
nix develop

# Build
cabal build

# Run tests
cabal test

# Run examples
cabal run example-all
cabal run example-structured-logging
cabal run example-type-reflection
cabal run example-functoid
```

### Without Nix

Requirements:
- GHC 9.2+ (for GHC2021 language)
- Cabal 3.6+

```bash
cd haskell/metaprogramming-challenges
cabal build
cabal test
cabal run example-all
```

---

## Project Structure

```
haskell/metaprogramming-challenges/
├── src/
│   └── Metaprogramming/
│       ├── StructuredLogging.hs   # Challenge 1
│       ├── TypeReflection.hs      # Challenge 2
│       └── Functoid.hs            # Challenge 3
├── examples/
│   ├── StructuredLoggingExample.hs
│   ├── TypeReflectionExample.hs
│   ├── FunctoidExample.hs
│   └── AllExamples.hs
├── test/
│   ├── Main.hs
│   └── Test/
│       ├── StructuredLogging.hs
│       ├── TypeReflection.hs
│       └── Functoid.hs
├── metaprogramming-challenges.cabal
├── flake.nix
└── README.md
```

---

## Implementation Techniques

### Compile-Time Metaprogramming

1. **Template Haskell**: AST manipulation for variable capture
2. **Type-level strings**: `KnownSymbol` for parameter IDs
3. **GADTs**: Encoding arity and type information
4. **Typeable**: Runtime type representation

### Type System Features

1. **Higher-rank types**: `forall` quantification
2. **Type applications**: `@Type` syntax for explicit type arguments
3. **Type classes**: Modeling subtype relationships
4. **Phantom types**: Type-level information without runtime representation

### Runtime Techniques

1. **Type.Reflection**: Modern typed reflection API
2. **Aeson**: JSON serialization
3. **Dynamic typing**: `cast` for runtime type checking

---

## Comparison with Other Languages

### Challenge 1: Structured Logging

**Haskell Approach**:
- Template Haskell for compile-time variable capture
- Type-safe JSON serialization
- Zero runtime overhead for template parsing

**vs Python**: Runtime frame introspection (more dynamic)
**vs Rust**: Declarative macros (similar compile-time approach)
**vs C++**: Preprocessor macros (less type-safe)

**Advantages**:
- Fully type-safe
- Compile-time errors for missing variables
- Elegant quasi-quoter syntax

### Challenge 2: Type Reflection

**Haskell Approach**:
- Built on `Typeable` and `Type.Reflection`
- Type classes for subtyping
- Fingerprints for efficient equality

**Unique aspects**:
- Subtyping via type classes (more flexible than OOP inheritance)
- Compile-time and runtime type safety
- No RTTI overhead (information from type system)

**Limitations**:
- Different subtyping model than OOP languages
- Requires `Typeable` constraint

### Challenge 3: Functoid

**Haskell Approach**:
- GADTs for arity-indexed functoids
- Type-level strings for parameter IDs
- Type class for polymorphic invocation

**vs Rust**: Procedural macros allow annotations
**vs Scala**: Compiler reflection provides richer metadata
**vs C++**: Template metaprogramming similar approach

**Advantages**:
- Type-safe parameter IDs at compile time
- Clean API with type applications
- Extensible via type classes

---

## Key Insights

1. **Haskell excels at compile-time metaprogramming** through:
   - Template Haskell for AST manipulation
   - Type-level programming (GADTs, type families, DataKinds)
   - Powerful type system with higher-rank types

2. **Type classes provide flexible abstraction**:
   - Model "subtyping" in a functional way
   - Polymorphic interfaces without inheritance
   - Compile-time verification

3. **Trade-offs**:
   - Template Haskell can be complex
   - Different mental model from OOP (subtyping via type classes)
   - Excellent type safety but requires understanding advanced features

---

## Language Extensions Used

- `TemplateHaskell` - Compile-time code generation
- `QuasiQuotes` - Custom syntax extensions
- `GADTs` - Generalized Algebraic Data Types
- `TypeApplications` - Explicit type arguments
- `DataKinds` - Type-level data
- `KindSignatures` - Kind annotations
- `RankNTypes` - Higher-rank polymorphism
- `ScopedTypeVariables` - Explicit scoping
- `AllowAmbiguousTypes` - Type inference flexibility

---

## Challenge Status

| Challenge | Status | Implementation |
|-----------|--------|----------------|
| 1. Structured Logging | ✅ | Template Haskell + Aeson |
| 2. Type Reflection | ✅ | Typeable + Type.Reflection |
| 3. Functoid | ✅ | GADTs + type-level strings + Typeable |
| 3.1. Named Type Tags | ✅ | KnownSymbol with @"id" syntax |

All challenges successfully implemented as pure library solutions!

---

## Notes

**Haskell's strengths for metaprogramming**:
- Template Haskell provides full AST access
- Type system enables sophisticated compile-time guarantees
- Type classes offer flexible abstraction mechanisms
- Modern `Type.Reflection` provides type-safe runtime reflection

**Where Haskell differs**:
- No OOP-style inheritance (modeled via type classes instead)
- Template Haskell syntax less ergonomic than some macro systems
- Learning curve for advanced type-level programming

**Overall**: Haskell demonstrates that functional languages can achieve sophisticated metaprogramming through different mechanisms than imperative/OOP languages, often with stronger compile-time guarantees.
