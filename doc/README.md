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

A variable is declared using the `let` keyword. The syntax is `let name = expression`. The expression is evaluated and the result is assigned to the variable.

```qiqe
# Variables are immutable
let x = 1
```

The variables are immutable. It means that you can't change the value of a variable.

```qiqe
let x = 1
let x = add x 1 # Error because x is immutable
```

### Functions

The syntax of functions is inspired by untyped lambda calculus. The syntax is `\arg1 arg2 ... argn. body`. The arguments are separated by spaces. The body is an expression.

> **INFO:** The body of a function is an expression, not a block of expressions.

> **TODO:** At the moment, it will works like a block of expressions using `where` keyword like in Haskell.

```qiqe
# Functions inspired by Lambda Calculus
let add = \x y. add x y
```

#### Currying

Currying is a way to create functions with more than one argument. For example, if you have a function `add` that takes two arguments, you can create a function `succ` that adds one to a number by using currying.

```qiqe
# Currying
let add = \x y. add x y
let succ = add 1

succ 2 # Returns 3
```

### Operators

> **TODO**: The operators are not yet implemented. Only the operators `>>`, `<<`, `|>`, `<|`, and `,` (cons operator) will be implemented, and the other operators are implemented as functions.

### Control Flow

In contrast to imperative languages, the control flow is done using expressions like ternary operator in C/C++ or Javascript.

```qiqe
# If
if eq x 1
  then 1
  else 2
```

It is necessary that the expressions in the same layer have the same indentation.

```qiqe
# If
if eq x 1
  then 1
else 2 # Error because the indentation is different
```

### Types

This language is a non-typed language. It means that you don't have to specify the type of a variable or a function. And it also means that you can use a variable or a function in any context. It language compiles to javascript. So, it is a dynamically typed language.

The types are:

- Integer
- Float
- Boolean
- String

> TODO: Add iterable type like List or Tuple. The implementation of the iterable type is not decided yet, but it will be thought using cons operator.

## Prelude

The prelude is a module that is imported by default in every file. It contains the basic functions.

### Functions

#### Basic

- `id`: Identity function

```qiqe
id x # Returns x
```

- `constant`: Constant function that returns the first argument

```qiqe
constant 1 2 # Returns 1
```

- `flip`: Function that flips the order of the arguments of a function

```qiqe
let constantFlipped = flip constant
constantFlipped 1 2 # Returns 2
```

#### Comparison

- `eq`: Equality function
- `neq`: Inequality function
- `lt`: Less than function
- `gt`: Greater than function
- `lte`: Less than or equal function
- `gte`: Greater than or equal function

```qiqe
eq 1 1 # Returns true
neq 1 1 # Returns false
lt 1 2 # Returns true
gt 1 2 # Returns false
lte 1 2 # Returns true
gte 1 2 # Returns false
```

#### Logic

- `not`: Negation function
- `and`: Logical and
- `or`: Logical or

```qiqe
not true # Returns false
and true false # Returns false
or true false # Returns true
```

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
- `neg`: Negation function
- `min`: Minimum value between two numbers
- `max`: Maximum value between two numbers
- `floor`: Floor function
- `ceil`: Ceil function
- `round`: Round to the nearest integer, e.g. 1.5 -> 2, 1.4 -> 1
- `truncate`: Round to the nearest integer towards zero, e.g. 1.5 -> 1, 1.4 -> 1

```qiqe
add 1 2 # Returns 3
sub 1 2 # Returns -1
mul 1 2 # Returns 2
div 1 2 # Returns 0.5
mod 1 2 # Returns 1
pow 2 3 # Returns 8
sqrt 4 # Returns 2
abs -1 # Returns 1
succ 1 # Returns 2
pred 1 # Returns 0
neg 1 # Returns -1
min 1 2 # Returns 1
max 1 2 # Returns 2
floor 1.5 # Returns 1
ceil 1.5 # Returns 2
round 1.5 # Returns 2
truncate 1.5 # Returns 1
```

#### String

- `concat`: Concatenate two strings
- `show`: It converts a value to a string
- `length`: It returns the length of a string. If the argument is not a string, it uses show to convert it to a string.

```qiqe
concat "Hello" " World!" # Returns "Hello World!"
show 1 # Returns "1"
length "Hello" # Returns 5
```

## Contributing in the design

If you want to contribute in the design of the language, you can open an issue or a pull request. You can also contact me at [Twitter](https://twitter.com/fabianmativeal).
