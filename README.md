Grow Your Own Type System
=========================

**Compilation instructions (note to self)**: `cd` into the `extensible_rows` subdirectory and run `dune build` there.     

This repository contains implementations of different type systems in OCaml.

It is meant to help out anyone who wants to learn more about advanced type systems and
type inference or experiment by extending or implementing their own. The implementations
are minimal and contain code that is (hopefully) simple and clear.

-   [**algorithm_w**](https://github.com/tomprimozic/type-systems/tree/master/algorithm_w)
    contains one of the most basic yet efficient implementation of Damas-Hindley-Milner
    type inference algorithm (used in functional languages such as OCaml, Haskell and Elm)
    called *Algorithm W*. Uses references to simulate type substitutions and assigns
    ranks/levels to type variables to simplify let-generalization.

-   [**extensible_rows**](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows)
    extends **algorithm_w** with type inference for extensible records/rows
    with scoped labels, based on Daan Leijen's excellent [paper][extensible_rows]. Although
    this is just one way of implementing extensible records, it's extremly simple and
    surprisingly useful, and was incorporated into the programming language
    [Elm](http://elm-lang.org/learn/Records.elm).

-   [**extensible_rows2**](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows2)
    is an optimized implementation of **extensible_rows**.

-   [**refined_types**](https://github.com/tomprimozic/type-systems/tree/master/refined_types)
    is an experiment that extends the HM type system with dependent types in the form of function
    contracts. It uses an external automatic theorem prover to verify that function contracts are
    satisfied, to prevent many of the most common software errors, such
    as division by zero and out-of-bounds array access.
