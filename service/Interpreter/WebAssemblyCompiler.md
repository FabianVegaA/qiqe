# WebAssembly Compiler for Functional Language

This document describes the WebAssembly compiler implementation that transforms a functional programming language into WebAssembly Text (WAT) format.

## Overview

The WebAssembly compiler (`CompilerWasm.hs`) is built on top of the existing JavaScript compiler architecture, sharing the same lexer and parser but generating WebAssembly instead of JavaScript code.

## Architecture

### Compilation Pipeline

```
Source Code → Lexer → Parser → AST → WebAssembly Compiler → WAT Code
```

### Key Components

1. **Type System**: WebAssembly requires explicit types, so the compiler includes type inference
2. **Context Management**: Tracks local variables, function names, and lambda IDs
3. **Instruction Generation**: Converts functional language constructs to WebAssembly instructions
4. **Module Wrapper**: Generates a complete WebAssembly module with exports and imports

## Type System

The compiler supports the following WebAssembly types:

- `I32Type`: 32-bit integers (also used for booleans and pointers)
- `F32Type`: 32-bit floating-point numbers
- `F64Type`: 64-bit floating-point numbers
- `FuncType [WasmType] [WasmType]`: Function types with parameter and return types

## Language Feature Mapping

### Literals

| Functional Language | WebAssembly |
|-------------------|-------------|
| `42` | `i32.const 42` |
| `3.14` | `f32.const 3.14` |
| `true` | `i32.const 1` |
| `false` | `i32.const 0` |
| `"hello"` | `i32.const <hash>` (pointer to string) |

### Control Flow

#### Conditionals
```haskell
-- Input: if true then 1 else 0
-- Output:
i32.const 1
(if (result i32)
  (then i32.const 1)
  (else i32.const 0)
)
```

### Functions

#### Function Definition
```haskell
-- Input: let add = \x -> x + 1
-- Output:
(func $add (param $x i32) (result i32)
  local.get $x
  i32.const 1
  i32.add
)
```

#### Function Application
```haskell
-- Input: add 5
-- Output:
i32.const 5
call $add
```

### Lambda Expressions

Lambda expressions are converted to named functions with unique identifiers:

```haskell
-- Input: (\x -> x * 2) 5
-- Output:
(func $lambda_0 (param $x i32) (result i32)
  local.get $x
  i32.const 2
  i32.mul
)

i32.const 5
call $lambda_0
```

## WebAssembly Module Structure

The generated WebAssembly module includes:

```wat
(module
  ;; Type definitions
  (type $func_type (func (param i32) (result i32)))
  
  ;; Linear memory for strings and complex data
  (memory $mem 1)
  (export "memory" (memory $mem))
  
  ;; Function table for indirect calls (closures)
  (table $func_table 100 funcref)
  
  ;; Helper functions for functional operations
  (func $pipe_i32 ...)
  (func $rpipe_i32 ...)
  
  ;; User-defined functions
  ;; ...
  
  ;; Main function (exported)
  (func $main (result i32) ...)
  (export "main" (func $main))
)
```

## Usage

### Basic Compilation

```haskell
import Interpreter.CompilerWasm
import Interpreter.Parser
import Interpreter.Lexer

compileToWasm :: String -> Either CompileErrorWasm Text
compileToWasm source = do
  tokens <- lexer source
  ast <- parse tokens
  compileWasm ast
```

### Example Usage

```haskell
-- Compile a simple function
let source = "{ let double = \\x -> x * 2; double 21 }"
case compileToWasm source of
  Left err -> print err
  Right wasmCode -> writeFile "output.wat" (T.unpack wasmCode)
```

## Advanced Features

### Higher-Order Functions

The compiler supports higher-order functions through function tables and indirect calls:

```haskell
-- Input: let apply = \f -> \x -> f x
-- Generates function table entries for closures
```

### Recursion

Recursive functions are supported with proper tail-call optimization where possible:

```haskell
-- Input: let factorial = \n -> if n <= 1 then 1 else n * factorial (n - 1)
-- Generates recursive WebAssembly function
```

### Closure Conversion

Closures are converted to function table entries with environment capture:

1. Free variables are identified
2. Environment is allocated in linear memory
3. Function reference points to table entry
4. Captured variables are passed as additional parameters

## Memory Management

### String Handling

Strings are stored in linear memory with a simple hash-based allocation:
- Hash function generates unique addresses
- String data is stored at calculated offsets
- Functions return pointers (i32) to string data

### Heap Allocation

For complex data structures, the compiler provides:
- Simple bump allocator
- Garbage collection hooks (for future implementation)
- Reference counting for shared data

## Error Handling

The compiler provides detailed error messages for:

- `TypeMismatchWasm`: When types don't match in expressions
- `UndefinedVariableWasm`: When referencing unknown variables
- `UnevaluatedASTWasm`: When encountering unsupported language constructs
- `FailedCompilerWasm`: General compilation failures

## Performance Considerations

### Optimization Strategies

1. **Constant Folding**: Compile-time evaluation of constant expressions
2. **Dead Code Elimination**: Removal of unused functions and variables
3. **Tail Call Optimization**: Converting recursive calls to loops
4. **Inlining**: Small function inlining for performance

### Memory Usage

- Local variables are efficiently mapped to WebAssembly locals
- Global constants are stored in WebAssembly globals
- Stack-based evaluation minimizes temporary allocations

## Testing

### Test Suite

The `CompilerWasmTest.hs` module provides comprehensive tests:

```haskell
runAllTests :: IO ()
-- Runs tests for:
-- - Basic literals
-- - Conditionals
-- - Functions
-- - Lambda expressions
-- - Complex expressions
-- - Recursion
-- - Higher-order functions
```

### Example Test Cases

```haskell
testBasicLiterals    -- Tests: 42, 3.14, true, "hello"
testConditionals     -- Tests: if-then-else expressions
testFunctions        -- Tests: function definitions and calls
testLambdas          -- Tests: lambda expressions and closures
testRecursion        -- Tests: recursive functions
```

### Benchmarking

Performance testing with various program sizes:

```haskell
benchmarkCompilation samplePrograms
-- Measures compilation time and output size
```

## Integration with Existing System

### Compatibility

The WebAssembly compiler is designed to be drop-in compatible with the existing JavaScript compiler:

- Same AST representation
- Same parser and lexer
- Similar error handling patterns
- Consistent API design

### Migration Path

To migrate from JavaScript to WebAssembly compilation:

1. Replace `Interpreter.Compiler` imports with `Interpreter.CompilerWasm`
2. Change `compile` calls to `compileWasm`
3. Update error handling for WebAssembly-specific errors
4. Modify output handling for WAT format instead of JavaScript

## Future Enhancements

### Planned Features

1. **Advanced Type System**: Support for custom types and type inference improvements
2. **Garbage Collection**: Automatic memory management for complex data structures
3. **SIMD Support**: Vector operations for mathematical computations
4. **Threading**: WebAssembly threading support for parallel execution
5. **WebAssembly Binary**: Direct binary generation instead of text format

### Optimization Opportunities

1. **Register Allocation**: Better local variable management
2. **Control Flow Optimization**: More efficient branching
3. **Memory Layout**: Optimized data structure representation
4. **Interop**: Better integration with JavaScript and other languages

## Debugging and Development

### WAT Output Inspection

The generated WAT code can be inspected and debugged using:
- WebAssembly text tools (`wat2wasm`, `wasm2wat`)
- Browser developer tools
- Standalone WebAssembly runtimes

### Common Issues

1. **Type Mismatches**: Ensure consistent types across expressions
2. **Stack Underflow**: Verify expression evaluation order
3. **Memory Access**: Check bounds for linear memory operations
4. **Function Signatures**: Ensure parameter counts match function definitions

## Conclusion

The WebAssembly compiler provides a robust foundation for compiling functional programming languages to WebAssembly, offering:

- Type safety through static analysis
- Efficient code generation
- Comprehensive error handling
- Easy integration with existing systems
- Extensible architecture for future enhancements

This implementation demonstrates how a JavaScript compiler can be successfully adapted to target WebAssembly while maintaining the same language semantics and developer experience.