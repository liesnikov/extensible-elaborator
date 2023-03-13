---
author: Bohdan Liesnikov
institute: TU Delft
title: Extensible elaborators
date: November 22, 2022
classoption: "aspectratio=169"
fontsize: 12pt
navigation: empty
theme: default
colortheme: dove
fonttheme: structuresmallcapsserif
section-titles: true
listings: true
mainfont: 'Source Serif 4'
sansfont: 'Source Sans 3'
monofont: 'Source Code Pro'
---

So, you want to build a compiler.

\pause

A compiler for a dependently-typed language.

\pause

You don't want to bake everything in just yet.
Great, let's make it extensible and figure things out as we go!

\pause

Well, maybe not _completely_ extensible.  
We can settle for extensible "on the surface".

# Bit of background

## How is your compiler structured

![](./dependent-types-compiler.pdf)

## How is the typechecker structured

![](./dependent-types-typechecker.pdf)

* not interested in the compilation part
* talking dependently-typed (DT) languages in particular

## Can we tackle the parser?

![](./dependent-types-typechecker.pdf)

* parsing is an old and hard problem
* in modern DT languages one either has a custom syntax declarations or proper macros
* let's not go there today

## Can we tackle the core?

![](./dependent-types-typechecker.pdf)

* core rules correspond to the encoded logic
* modifying core can violate soundness
* generally speaking, not something you want to mess with

## Elaborator under attack

![](./dependent-types-general.pdf)

## Problem statement

* want to find a principled design for an extensible elaborator.
* *don't want to* create a DSL to only express "correct" type theories

# Going deeper into the elaborator

## How does an elaborator work

* essentially do type-checking
* more liberal
* if something isn't immediately obvious  
  constrain the unknowns such that it typechecks

## Example: type classes

* during initial typechecking of the body you don't care about a particular instance
* just that it does exist
* create a constraint to look for one
* do a search later
* maybe block the problem if you need to reduce something that involves the instance

## Example: infer an argument

```
f : ∀ (A : Type) (a : A) (n : Nat) -> Vec A n

f _ (zero) (succ zero)
```

How do we go around typechecking it?

* lookup `f`
* lookup `zero : Nat`
* lookup `succ : Nat -> Nat`
* constraint the type of implicit parameter `_` to `Type`
* it has to be `Nat`
* done


## Example: infer an argument

```
f : ∀ (k : Nat) (A : F k) (a : A) (n : Nat) -> Vec A n

f k _¹ (C _² _³) (succ zero)
```

* replace `_¹` with `?¹`
* we know that `?¹ : F k`
* elaborating `(C _² _³)` yields type `T ?² ?³`
* produce a constraint `?¹ ~ T ?² ?³`

## How do constraints work

* typically a compiler has a constraint language
* Haskell has a "simple" one:
  ```
  W = empty
    | W1, W2 # conjunction
    | C t1 .. tn # type class constraint
    | t1 ~ t2 # equality constraint
    | ∀a1..an. W1 => W2 # implication constraint
  ```

## How do constraints work

* Agda has a more complex one
```
W = ValueCmp t1 t2 # eq comparison
  | ElimCmp typ t1 e1 e2 # elim comparison
  | SortCmp s1 s2 # (type) sort comparisons
  | LevelCmp l1 l2 # (type) level comparisons
  | UnBlock m1 # Meta created for a term blocked
  | FindInstance m1 c # type class instances
  | CheckFunDef ... # couldn't check a function def because
  | UnquoteTactic ...
  ... # plenty more
```

## How do constraints work

* aiming for something in-between in the core + your extensions

```
W = empty
  | W1, W2
  | IsDatatypeConstructor
  | EqualityComparison
  | MetaSolved
  ...
  # more to come?
```

## How do constraints work

* create and collect constraints while typechecking
* solve them one by one in the process or afterwards

## Core idea

What if we open this constraint language to the "power users"?

## Why

* get a relatively compact core of the elaborator
* build features around it as "extensions" or "plugins"
* allow cheaper experiments with the language
* main inspirations: Haskell [@jonesPracticalTypeInference2007 ; @ghcdevelopmentteamGlasgowHaskellCompiler], Matita [@tassiBiDirectionalRefinementAlgorithm2012]

Bottom line: this is a design study

# Gory implementation details

## Baseline language

* DT language
* Pi, Sigma types
* inductive types
* case-constructs for elimination (not case-trees)
* some other extra features, ignored for now

## Additions

::: incremental

* implicit arguments  
  for now aiming for a placeholder term `_` (maybe n-ary `_`?)
* type classes
  * can we derive things?
* subtyping by coercion
* taking suggestions

:::


## Design choices and implementation ideas

* constraints are async procedure calls
* metas are communication channels for async computations
* we have metas for terms  
  but not for modalities or other language constructs just yet
* in the process of solver implementation

## Design choices and implementation ideas

* the extension writer communicates whether the problem is blocking or not by freezing the rest of the typechecking
* each constraint raised gets an opportunity to be solved/simplified immediately by the user-supplied solver
* if it doesn't - we can freeze the problem and return a meta in place of the typechecked solution

## Design overview

![](./architecture-diagram.pdf)

## Open questions

::: incremental

* how to encode solver patterns and order?
* can we link dynamically?

:::


## how to encode solver priorities

Current idea:

* specify a (pre-) order in which the solvers should run  
  i.e. type classes run after name disambiguation

## how does dynamic linking interfere with what we can do

* there is machinery in Haskell to declare interfaces [@pangPluggingHaskell2004]
* what should the interface look like?
```
  data SolverTag = ...
  
  type Solver = ConstraintF c -> ( Set (ConstraintF c)
                                 , Map MetaId Term )
  
  data PluginInterface = Plugin {
    solvers :: Map SolverTag Solver,
    solverTags :: [SolverTag],
    priorities :: [(SolverTag, SolverTag)]
  }
```

## how does dynamic linking interfere with what we can do

```
  instance ConstraintMap constraintTag ConstraintType where
  ...
  
  data Constraints = Constraints {
    constraintTags :: Set String,
  }
  ```


## Prior work

* Haskell
  * plugins
  * hooks
  * was supposed to get dependent types
* Coq
  * plugins don't really have an interface
  * not restricted in any way, if you go into ml space
  * very confusing
* Lean
  * uses macros to redefine symbols
  * uses reflection and typechecking monads to define custom elaboration procedures
* TypOS
  * you have to buy into a whole new discipline
  * we hope to keep things a bit more conventional engineering-wise

## Closing slide

![](./dependent-types-general.pdf)


## References {.allowframebreaks}

\bibliographytrue
\printbibliography[heading=none]

<!-- Local Variables: -->
<!-- mode: markdown; reftex -->
<!-- reftex-cite-format: biblatex -->
<!-- reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/paper/bib.bib") -->
<!-- End: -->
