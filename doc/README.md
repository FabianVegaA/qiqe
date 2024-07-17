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
      - [Composition](#composition)
      - [Pipe](#pipe)
    - [Control Flow](#control-flow)
    - [Types](#types)
      - [List](#list)
  - [Libraries](#libraries)
  - [Examples](#examples)
  - [Contributing in the design](#contributing-in-the-design)

## Introduction

Qiqe is a general purpose functional language. It is based in untyped lambda calculus. It is a compiled language that compiles to JavaScript. It is designed to be used in the browser.

## Syntax

### Comments

Only the line comments are allowed. The syntax is `#`. The comment starts with `#` and ends at the end of the line.

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

#### Composition

The composition operators are `>>` and `<<`. The operator `>>` is the composition operator from right to left. The operator `<<` is the composition operator from left to right.

```qiqe
f >> g # Equivalent to \x. g (f x)
f << g # Equivalent to \x. f (g x)
```

#### Pipe

The pipe operator are `|>` and `<|`. The operator `|>` is the pipe operator from left to right and vice versa.

```qiqe
x |> f # Equivalent to f x
f <| x # Equivalent to f x
```

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

#### List

The list in Qiqe are like linked lists, composed by `cons` and `nil`. The `cons` is a function that takes tree arguments: the head, the tail and a boolean value, example:

```qiqe
let cons = \head tail x. if x then head else tail
let head = \list. list true
let tail = \list. list false
let null = \list. eq list nil

# An example of a list
let list = cons 1 (cons 2 (cons 3 nil))

head list # Returns 1
tail list # Returns cons 2 (cons 3 nil)
```

Qiqe has a syntax sugar for lists. The syntax is `[1, 2, 3]`. The previous example can be written as:

```qiqe
let list = [1, 2, 3]
```

> For now, the nil is a value and it is not implemented as a lambda calculus function.

## Libraries

- [Base](./../qiqe/library/std.qq): This library contains the basic functions.
- [List](../qiqe/library/list.qq): This library contains the functions for lists.

## Examples

- Hello World:
  ```qiqe
  print "Hello, World!"
  ```
- Fibonacci:

  ```qiqe
  let fib = \n. if eq n 0
    then 0
    else if eq n 1
      then 1
      else add (fib (sub n 1)) (fib (sub n 2))
  ```

- [Stair](./examples/stairs.qq)
- [Triangle](./examples/triangle.qq)

## Contributing in the design

This is a experimental language that born in my free time, and it is not finished yet, but if you want to contribute in the design of the language, you can open an issue or a pull request. You can also contact me at [Twitter](https://twitter.com/fabianmativeal).
