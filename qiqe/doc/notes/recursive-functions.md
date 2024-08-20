# Recursive functions in let-in expressions

When I was implementing the `let-in` expression in Qiqe my first idea was a simple transformation such as:

```qiqe
let a =
    let f = \x. x
    in f 1

# to

a = (\f. f 1) (\x. x)
```

For multiple definitions, I thought about a similar transformation:

```qiqe

c = let
    a = 1;
    b = 2
  in add a b

# to

c = (\a b. add a b) 1 2
```

And so on...

However, what about recursive functions?
If I use the same approach as above, I will have a problem. For example:

```qiqe
let f = let
      g = \x. g x
    in g 1

# to
let f = (\g. g 1) (\x. g x)
```

If I apply the reduction steps, I will have:

```qiqe
f = (\g. g 1) (\x. g x)
  = (\x. g x) 1
  = g 1
```

What happened to `g`? Where is the definition of `g`?

I needed to think more deeply about this issue. While using environment variables would be the easiest approach, I wanted to implement this using only lambda calculus, as performance is not a concern at this stage. The solution I discovered was to use a fixed-point combinator

The Y combinator is a fixed-point combinator that allows the definition of recursive functions in lambda calculus. It is defined as:

```qiqe
Y = \f. (\x. f (x x)) (\x. f (x x))
```

The Y combinator is a function that receives a function `f` and returns a function that applies `f` to itself. The `f` function must be a higher-order function that receives a function as an argument. The Y combinator is used to define recursive functions in lambda calculus.

For Qiqe, I implemented a similar combinator, the `U` combinator. The `U` combinator is a fixed-point combinator that allows the definition of recursive functions in Qiqe. It is defined as:

```qiqe
U = \u. (u u)
```

Internally, this is called `delta` in the Qiqe implementation. For example, the recursive function `sum` can be defined as:

```qiqe

let sum = let
    go = \acc n. if eq n 0
        then acc
        else go (add acc n) (sub n 1)
    in go 0

# to
let sum = (\go. go 0) ((\u. (u u)) (\self. (\go. <exp>)) (\x. (self self) x))
#          ^^^^^^^^^   ^^^^^^^^^^                ^^^^^    ^^^^^^^^^^^^^^^^^
#           Evaluated     delta             The recursive     An auxiliary
#          expression   combinator             function        function
#              (1)         (2)                    (3)             (4)
# (1) The evaluted expression is passed in the `in` part of the let-in expression
# (2) The delta combinator is applied to itself
# (3) The body of the recursive function is passed to the delta combinator
# (4) This is the auxiliary function that will be used for modification
#     of the recursive function with the delta combinator
```

## References

- [Many faces of the fixed-point combinator](https://okmij.org/ftp/Computation/fixed-point-combinators.html)
