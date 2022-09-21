---
author: Bohdan Liesnikov
institute: TU Delft
title: Extensible elaborators
subtitle: Seminar talk
date: September 21, 2022
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

We would like to have extensible languages.

\pause

Well, maybe not _completely_ extensible.  
We can settle for extensible "on the surface".

# Bit of background

## How the compiler is structured

![](./dependent-types-compiler.pdf)

## How the typechecker is structured

![](./dependent-types-typechecker.pdf)

* Not interested in the compilation part
* Talking dependently-typed (DT) languages in particular

## Can we tackle the parser?

![](./dependent-types-typechecker.pdf)

* Parsing is an old and hard problem
* In modern DT languages one either has a custom syntax declarations or proper macros
* Let's not go there today

## Can we tackle the core?

![](./dependent-types-typechecker.pdf)

* Core rules correspond to the encoded logic
* Modifying core can violate soundness
* Generally speaking, not something you want to mess with

## Elaborator under attack

![](./dependent-types-general.pdf)

## Problem statement

We would like to have extensible languages.

Well, maybe not _completely_ extensible.  
We can settle for extensible ~~"on the surface"~~ "in the middle".

# Going deeper into the elaborator

## How does an elaborator work

* We essentially do type-checking
* More liberal
* If something isn't immediately obvious  
  constrain the unknowns such that it typechecks

## "Simple" example: type classes

* during initial typechecking of the body you don't care about a instance
* just that it does exist
* create a constraint to look for one
* do a search later

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


## Example: infer an argument (complex)

```
f : ∀ (k : Nat) (A : F k) (a : A) (n : Nat) -> Vec A n

f k _ t (succ zero)
```

* replace `_` with `?¹`
* look up `k` yields not fully elaborated term *yet*
* you get a constraint `?¹ ~ F ?²`
* looking up `t` yields type `G`
* produce a constraint `F ?² ~ G`

## How do constraints work

* Typically a compiler has a constraint language
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

* Create and collect constraints while typechecking
* Solve them one by one in the process or afterwards

## Core idea

What if we open this constraint language to the "power users"?

## Why

* Get a relatively compact core of the elaborator
* Build features around it as "extensions" or "plugins"
* Allow cheaper experiments with the language
* Main inspirations: Haskell [@jonesPracticalTypeInference2007 ; @ghcdevelopmentteamGlasgowHaskellCompiler], Matita [@tassiBiDirectionalRefinementAlgorithm2012]

Bottom line: this is a design study

# Gory implementation details

## Baseline language

* DT language
* Pi, Sigma types
* inductive types
* case-constructs for elimination
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


## Implementation space

::: incremental

* how to encode solver patterns and order?
* are metavariables special enough to bake them in explicitly?
* should we make syntax an open data-type?
* how does dynamic linking interfere with what we can do?

:::


## how to encode solver priorities

Current idea:

* at plugin writing-time "import" modules with existing solvers
* reuse pattern-matching machinery from the typechecker
* specify a (pre-) order in which the solvers should run  
  i.e. type classes run after name disambiguation

## are metavariables special enough to bake them in explicitly

* for now -- probably, yes
* similar problem to "open datatype"


## should we make syntax an open data-type

* Would be great, not clear how much accidental complexity this brings.
* Currently the code is written using `unbound-generics` [@alekseykligerUnboundgenerics2022]. Needs Generic instances, which might be problematic to get.
* Alternatively, "open" datatype on top of the existing internal one.
* Trees that grow? [@najdTreesThatGrow2017]

## how does dynamic linking interfere with what we can do

* There is machinery in Haskell to declare interfaces [@pangPluggingHaskell2004]
* what should the interface look like?
* ```
  data SolverTag = ...
  
  type Solver = Problem -> (Set Problem, Substitution)
  
  data PluginInterface = Plugin {
    solvers :: Map SolverTag Solver,
    solverTags :: [SolverTag],
    priorities :: [(SolverTag, SolverTag)]
  }
  ```


## Prior work

* Haskell
  * Plugins
  * Hooks
  * Was supposed to get dependent types
* Coq
  * Plugins don't really have an interface
  * Not restricted in any way, if you go into ml space
  * Very confusing
* Lean
  * Uses macros to redefine symbols
  * Uses reflection and typechecking monads to define custom elaboration procedures

## Closing slide

![](./dependent-types-general.pdf)


## References {.allowframebreaks}

\bibliographytrue
\printbibliography[heading=none]

## Backup

* Hey, we have backup slides!

<!-- Local Variables: -->
<!-- mode: markdown; reftex -->
<!-- reftex-cite-format: biblatex -->
<!-- reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/paper/bib.bib") -->
<!-- End: -->
