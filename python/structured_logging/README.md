# Python Structured Logging - Challenge 1

This implementation demonstrates **Effortless Structured Logging** in Python without compiler plugins or language modifications. It uses runtime frame introspection to extract both the template and variable values from f-strings.

## Overview

When you write:
```python
logger.log(f"Hello {user}, your balance is {balance}")
```

The logger automatically extracts and outputs:
```json
{
  "template": "Hello %user%, your balance is %balance%",
  "args": {
    "user": "John",
    "balance": 42
  }
}
```

## How It Works

The implementation leverages Python's powerful runtime introspection capabilities:

1. **Frame Introspection**: Uses `inspect.currentframe()` to capture the caller's execution context
2. **Source Code Extraction**: Reads the actual source line where `log()` was called
3. **F-String Parsing**: Uses regex to extract variable names from the f-string syntax
4. **Variable Resolution**: Evaluates variables in the caller's local and global scope
5. **Structured Output**: Generates JSON with template and resolved values

### Key Components

- **StructuredLogger**: Main class that implements the logging functionality
- **_get_source_line()**: Extracts the source code from the calling frame
- **_extract_template_and_args()**: Parses f-strings and resolves variable values

## Usage

### Basic Example

```python
from logger import logger

user = "John"
balance = 42
logger.log(f"Hello {user}, your balance is {balance}")
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

### Advanced Features

#### Multiple Variable Types
```python
name = "Alice"
age = 30
score = 95.5
active = True
logger.log(f"User {name} is {age} years old with score {score} (active: {active})")
```

#### Expressions in F-Strings
```python
x = 10
y = 5
logger.log(f"The sum of {x} and {y} is {x + y}")
```

Output:
```json
{
  "template": "The sum of %x% and %y% is %x + y%",
  "args": {
    "x": 10,
    "y": 5,
    "x + y": 15
  }
}
```

#### Complex Objects
```python
data = {"key": "value", "count": 123}
items = [1, 2, 3]
logger.log(f"Data: {data}, Items: {items}")
```

## Running the Example

```bash
cd python/structured_logging
python example.py
```

## Implementation Details

### Why This Works in Python

Python provides excellent runtime introspection capabilities that make this implementation possible:

1. **inspect module**: Gives access to live frame objects with source code information
2. **f_locals and f_globals**: Provide access to the caller's variable scope
3. **eval()**: Allows dynamic evaluation of expressions in the caller's context
4. **Source code access**: Python can read its own source files at runtime

### Limitations

- Requires source code to be available (won't work with compiled-only distributions)
- F-string must be on a single line
- Complex multi-line f-strings may need special handling
- Performance overhead from frame introspection (not suitable for hot paths)

### Comparison with Other Approaches

Unlike macro-based approaches in languages like Rust or Scala, this is a **pure library solution** with no:
- Compiler plugins
- Build-time code generation
- Language extensions
- Special syntax

## Technical Notes

### Memory Management

The implementation properly manages frame references to avoid reference cycles:
```python
try:
    # Work with frames
    ...
finally:
    del caller_frame
    del frame
```

### Error Handling

The logger uses assertions for critical failures and graceful fallbacks for variable resolution:
- Assertions ensure frames are available
- Try-except blocks handle variable evaluation errors
- Multiple fallback strategies for source code extraction

## Conclusion

This implementation proves that Python can achieve structured logging through library-level metaprogramming, using its powerful runtime introspection capabilities. While it has some limitations compared to compile-time macro systems, it demonstrates that sophisticated metaprogramming features can be implemented without language modifications.

## Challenge Status

✅ **Implemented** - Pure library solution using frame introspection
