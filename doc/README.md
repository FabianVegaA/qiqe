# Qiqe Documentation

## Table of Contents

- [Qiqe Documentation](#qiqe-documentation)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Syntax](#syntax)
    - [Comments](#comments)
    - [Variables](#variables)
    - [Functions](#functions)
      - [Currying](#currying)
    - [Operators](#operators)
    - [Control Flow](#control-flow)
    - [Types](#types)
  - [Prelude](#prelude)
    - [Functions](#functions-1)
      - [Basic](#basic)
      - [Comparison](#comparison)
      - [Logic](#logic)
      - [Arithmetic](#arithmetic)
      - [String](#string)
  - [Contributing in the design](#contributing-in-the-design)

## Introduction

Qiqe is a general purpose functional language. It is inspired by Haskell, Elm and Lisp. It is a compiled language that compiles to javascript. It is designed to be used in the browser.

## Syntax

### Comments

```qiqe
# This is a comment
```

### Variables

```qiqe
# Variables are immutable
let x = 1
```

### Functions

```qiqe
# Functions inspired by Lambda Calculus
let add = \x y. x + y
```

#### Currying

Currying is a way to create functions with more than one argument. For example, if you have a function `add` that takes two arguments, you can create a function `succ` that adds one to a number by using currying.

```qiqe
# Currying
let add = \x y. x + y
let succ = add 1

succ 2 # Returns 3
```

### Operators

> **TODO**: Operators are not implemented yet. Only the operators `>>`, `<<`, `|>` and `<|` are implemented, and the others operators are implemented as functions.

### Control Flow

```qiqe
# If
if x == 1
  then 1
  else 2
```

### Types

This language is a non-typed language. It means that you don't have to specify the type of a variable or a function. And it also means that you can use a variable or a function in any context. It language compiles to javascript. So, it is a dynamically typed language.

The types are:

- Integer
- Boolean
- String

> TODO: Add Float type and an iterable type like List or Tuple. The implementation of the iterable type is not decided yet, but it will be thought using cons operator.

## Prelude

The prelude is a module that is imported by default in every file. It contains the basic functions.

### Functions

#### Basic

- `id`: Identity function
- `constant`: Constant function that returns the first argument
- `flip`: Function that flips the order of the arguments of a function

#### Comparison

- `eq`: Equality function
- `neq`: Inequality function
- `lt`: Less than function
- `gt`: Greater than function
- `lte`: Less than or equal function
- `gte`: Greater than or equal function

#### Logic

- `not`: Negation function
- `and`: Logical and
- `or`: Logical or

#### Arithmetic

- `add`: Addition function
- `sub`: Subtraction function
- `mul`: Multiplication function
- `div`: Division function
- `mod`: Modulus function
- `pow`: Power function
- `sqrt`: Square root function
- `abs`: Absolute value function
- `succ`: Successor to a number
- `pred`: Predecessor to a number

#### String

- `concat`: Concatenate two strings
- `show`: It converts a value to a string
- `length`: Length function.
  > If the argument is not a string, it uses show to convert it to a string.

## Contributing in the design

If you want to contribute in the design of the language, you can open an issue or a pull request. You can also contact me at [Twitter](https://twitter.com/fabianmativeal).
