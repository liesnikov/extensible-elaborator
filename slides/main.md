---
author: Bohdan Liesnikov and Jesper Cockx
title: Building an elaborator using extensible constraints
institute: TU Delft, Delft, Netherlands
date: June 12th, 2023
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

\faceA I want to implement a _dependently-typed_ language!

\pause \raggedleft
But do you know what it'll look like? \faceB

\pause \raggedright
\faceA Not completely, but I'll build it in a modular way!

\pause \raggedleft
Making the core modular is _very_ hard... \faceB

\pause \raggedright
\faceA I won't mess with the core then, just the elaborator!

## Slightly more formal

* Can we build an elaborator that is extensible?
* Core type-checker and language have stay the same
* Parsing is a solved problem
* Allow the developers and power-users to add new features to the source

## Elaborator under attack

![](./dependent-types-general.pdf)

## Constraints as a technique in Haskell

* typically a compiler has a constraint language
* Haskell has a "simple" one:
  ```
  W = empty
    | W1, W2 # conjunction
    | C t1 .. tn # type class constraint
    | t1 ~ t2 # equality constraint
    | ∀a1..an. W1 => W2 # implication constraint
  ```

## Constraints as a technique in Agda

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

## Graph of constraints constructors over the years

![](./agda-constraints.pdf)

## Show elaboration of a small example

# What concretely are we suggesting

## Our constraints design

* aiming for something in-between in the core + your extensions

```
CoreW = EqualityComparison t1 t2 ty m
      | IsDatatypeConstructor t
      | BlockedOnMeta m tc
      | FillInMeta m
      ...
```


## Base language

* DT language
* Pi, Sigma types
* inductive types with indexes
* case-constructs for elimination (not case-trees)

## Additions

::: incremental

* implicit arguments with placeholder terms
* type classes
* tactic arguments?
* subtyping by coercion?
* row types?

:::

## Current state

[github.com/liesnikov/extensible-elaborator](https://github.com/liesnikov/extensible-elaborator)

* there's a simple unifier implemented
* working on implicit arguments

## Open questions

* How much can such a unifier scale?
* How far can you push these kinds of extensions?
  i.e. can you model erasure inference?
* What if we allow plugins to have a custom store in the monad?
* Can we make the solver parallel?

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

[github.com/liesnikov/extensible-elaborator](https://github.com/liesnikov/extensible-elaborator)

# Backup slides

## Old architecture diagram

![](./architecture-diagram.pdf)

## How do you make sure the solvers run in the right order?

specify a (pre-) order in which the solvers should run i.e. type classes run after name disambiguation

## What does a plugin look like?

```haskell
  type PluginId = ...

  type Solver cs = forall m. (MonadSolver cs m) =>
                   (Constraint cs) ->
                   m Bool

  data Plugin cs = Plugin {
    handler :: Handler cs,
    solver :: Solver cs,
    symbol :: PluginId,
    pre :: [PluginId],
    suc :: [PluginId]
  }
```


## Why (bother with splitting)

* at the moment the biggest "usual" solver is a conversion checker
* it typically ranges around 1.7kloc in Idris, Lean, Coq
* in Agda also results in a lot of intricacies in the codebase
* chains of nested calls with logic spread around `compareAs`/`compareTerm`/`compareAtom`
* the need to manually catch and handle constraints at times `catchConstraint`/`patternViolation`

## Why (open it up)

* get a relatively compact core of the elaborator
* build features around it as "extensions" or "plugins"
* allow cheaper experiments with the language
* main inspirations: Haskell [@jonesPracticalTypeInference2007 ; @ghcdevelopmentteamGlasgowHaskellCompiler], Matita [@tassiBiDirectionalRefinementAlgorithm2012]

. . .

Bottom line: this is a design study

## References {.allowframebreaks}

\bibliographytrue
\printbibliography[heading=none]


<!-- Local Variables: -->
<!-- mode: markdown; reftex -->
<!-- reftex-cite-format: biblatex -->
<!-- reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/paper/bib.bib") -->
<!-- End: -->
