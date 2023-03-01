---
title: 'Extensible elaborator design'

output: pdf_document

documentclass: easychair
classoption:
    - a4paper
colorlinks: true

header-includes: |
    \author{Bohdan Liesnikov\inst{1} \and Jesper Cockx\inst{1}}
    \authorrunning{Liesnikov and Cockx}
    \titlerunning{Extensible elaborator design}
    \institute{TU Delft}
    \usepackage{todonotes}
---

\begin{abstract}

We present work-in-progress on a new design for compilers for dependently-typed languages based on the idea of an open datatype for constraints.
This allows for a more compact base elaborator implementation while enabling extensions to the type system.
We don't require modifications to the core of type-checker, therefore preserving safety of the language.

\end{abstract}


##### Introduction #####  {#section_introduction}

The usual design of a compiler for a dependently-typed language consist of three main parts: a parser, an elaborator, a core type-checker, and a back-end.
Some of the languages can omit some parts, like lack of the core type-checker in Agda.

Both the elaborator and the core type-checker can be divided into two parts: traversal of the terms and collection (followed by solving) of the constraints [@bruijnPleaWeakerFrameworks1991].
These can be found in all major languages like Idris, Coq, Lean, and Agda.

Idris has only [one kind of constraints](https://github.com/idris-lang/Idris2/blob/e673d05a67b82591131e35ccd50fc234fb9aed85/src/Core/UnifyState.idr) with the only two constructors being equality constraint for two terms and for two sequences of terms.
Both of these are [solved by the unifier](https://github.com/idris-lang/Idris2/blob/542ebeae97ed8b35ca1c987a56a61e98d4291a75/src/Core/Unify.idr#L1392-L1430).
It is defined in the module `Core.Unify` which spans over 1.5 thousand lines.

\todo{example from Lean}

* [the wrong thing ](https://github.com/leanprover/lean4/blob/0a031fc9bbb43c274bb400f121b13711e803f56c/src/Lean/Meta/Match/Basic.lean#L161) for Lean
* basic metavariable definitions https://github.com/leanprover/lean4/blob/30199745ad205dab58ff80bd8eb9b212ac1e765f/src/Lean/Meta/Basic.lean
* some unification tactic? https://github.com/leanprover/lean4/blob/30199745ad205dab58ff80bd8eb9b212ac1e765f/src/Lean/Meta/Tactic/UnifyEq.lean
* this seems like definitional equality checker? https://github.com/leanprover/lean4/blob/30199745ad205dab58ff80bd8eb9b212ac1e765f/src/Lean/Meta/Basic.lean#L1550-L1561

\todo{example from Coq}

* solving evars?? https://github.com/coq/coq/blob/4804c2b3479a447d75473b7d6b57be01bcb45cdf/pretyping/evarsolve.mli
* evared term type https://github.com/coq/coq/blob/110921a449fcb830ec2a1cd07e3acc32319feae6/engine/eConstr.mli* term type https://github.com/coq/coq/blob/c609f9b8549e7e9a946f3d783f71f7cdca35c8cc/kernel/constr.mli
* Unification https://github.com/coq/coq/blob/61ed5bf56871768ca020f119baa963b69ffe56f3/pretyping/unification.mli
* Unification for type inference https://github.com/coq/coq/blob/155688103c43f578a8aef464bf0cb9a76acd269e/pretyping/evarconv.mli


Agda differs somewhat from the others [a family of constraints](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Monad/Base.hs#L1064-L1092) that grew organically, currently, that's 17 constructors.

##### Problems with unifiers #####  {#section_unifier_problems}

Higher-order unification is notoriously hard to implement because it is undecidable in general.
The complexity stems from the desire of compiler writers to implement the most powerful unifier.
This code is also heavily used throughout the compiler, making it sensitive towards changes and hard to maintain and debug. \todo{footnote about Agda CI on cubical and stdlib, Coq on unimath}
Some of this complexity is unavoidable, but we can manage it better by splitting it up into small modular components.
In practice, this means that one doesn't have to fit together an always-growing conversion checker but can instead write different cases separately.
We again rely on the constraint solver machinery to distribute the problems to the fitting solvers.

An example from Agda's conversion checker is `compareAs` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L146-L218) which provides type-driven conversion checking.
The function is almost 90 lines long, and yet the vast majority of it is special cases of metavariables.
This function calls the `compareTerm'` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L255-L386), which itself is 130 lines.
`compareTerm'` calls the `compareAtom` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L419-L675).
Which itself is almost 200 lines of code.
Each of the above functions implements part of the "business logic" of the conversion checker.
But each of them contains a lot of code dealing with bookkeeping related to metavariables and constraints:
1. They have to throw and catch exceptions, driving the control flow of the unification.
2. They have to compute blocking tags that determine when a postponed constraint is retried.
3. They have to deal with cases where either or both of the sides equation or its type are either metavariables or terms whose evaluation is blocked on some metavariables.

This code is unintuitive and full of intricacies as indicated by [multiple](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L430-L431) [comments](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L521-L529).

Zooming in on the `compareAtom` function, the actual logic can be expressed in about [20 lines](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L530-L579) of simplified code. \todo{stripping out size checks, cumulativity, polarity, and forcing}

``` haskell
case (m, n) of
  (Pi{}, Pi{}) -> equalFun m n

  (Sort s1, Sort s2) -> equalSort s1 s2

  (Lit l1, Lit l2) | l1 == l2 -> return ()

  (Var i es, Var i' es') | i == i' -> do
      a <- typeOfBV i
      compareElims [] [] a (var i) es es'

  (Def f es, Def f' es') -> do
      a <- computeElimHeadType f es es'
      compareElims [] a (Def f []) es es'

  (Con x ci xArgs, Con y _ yArgs) | x == y -> do
      t' <- conType x t
      compareElims t' (Con x ci []) xArgs yArgs

  _ -> notEqual
```

\todo{get an example from Idris 2?}

This is precisely what we'd like the compiler developer to write, not to worry about the dance around the constraint system.

##### How do we solve this #####  {#section_solution}

##### Extension of the system to include open constraint datatype #####

###### Implicits ######

In this view, the elaborator for the application of a function doesn't have to know anything about the implicits at all.
The only thing we require is that the elaboration of the argument is called with the type information available.
This corresponds to how in bidirectional typing function application is done in the inference mode but the arguments are processed in checking mode.

```haskell
inferType (App t1 t2) = do
  (et1, Pi tyA tyB) <- inferType t1
  et2 <- checkType t2 tyA
  return (App et1 et2, subst tyB et2)
```

```haskell
checkType (Implicit) ty = do
  m <- createMeta
  raiseConstraint $ FillInTheTerm m ty
  return m
```

This metavariable in its own turn gets instantiated by a fitting solver.
The solvers match the shape of the type that metavariable stands for and handle it in a case-specific manner: instance-search for type classes, tactic execution for a tactic argument.
If it is a regular implicit, however, the only solver that's needed is a trivial one that checks that the metavariable has been instantiated indeed.
This is because a regular implicit should be instantiated by a unification problem encountered at some point later.
This serves as a guarantee that all implicits have been filled in.

Let us go through an example of the elaboration process for a simple term:

```
plus : {A : Type} -> {{PlusOperation A}} -> (a : A) -> (b : A) -> A

instance PlusNat : PlusOperation Nat where
  plus = plusNat

two = plus 1 1
```

We will step through the elaboration of the term `two`.

1. First, the pre-processor eliminates the implicits and type-class arguments.
   We end with the following declarations:
   ```
   plus : (impA : Implicit Type)
       -> TypeClass PlusOperation (deImp impA)
       -> (a : deImp impA) -> (b :  deImp impA)
       ->  deImp impA
   
   PlusNat = Instance {
       class = PlusOperation Nat,
       body = {plus = plusNat}}
   
   two = plus _ _ 1 1
   ```
2. We go into the elaboration of `two` now.
   The elaborator applies `inferType (App t1 t2)` rule four times and `checkType (Implicit) ty` twice on the two placeholders.
   The output of the elaborator is
   ```
   two = plus ?_1 ?_2 1 1
   ```
   And the state of the elaborator contains four more constraints:
   ```
   C1: FillInTheTerm ?_1 (Implicit Type)
   C2: FillInTheTerm ?_2 (TypeClass PlusOperation (deImp ?_1))`
   C3: EqualityConstraint ?_1 Nat Type`
   C4: EqualityConstraint ?_1 Nat Type`
   ```
   
   The first two correspond to implicit arguments.
   The latter two are unification problems rendered into constraints.

3. Now we step into the constraint-solving world.
   First, the unifier solves the latter two, instantiating `?_1` to `Nat`.
   Next, the type-class resolution launches a search for the instance, resolving `?_2` to the `PlusNat` instance.
   Finally, C1 is discarded as solved since `?_1` is already instantiated to `Nat`.

###### Tactic arguments ######

###### Coercion ######

###### Row types ######

##### Future work #####

There are some things we leave for future work.

* Implement erasure inference [@tejiscakDependentlyTypedCalculus2020]?
* Implement Canonical structures [@mahboubiCanonicalStructuresWorking2013]?
* Rendering of macros as constraints?
  Map a macro to an implicit term with the right kind of annotation in the type, to get the right expander as an elaboration procedure?
* Mapping constraint solving onto a concurrent execution model.
  Use LVars here [@kuperLatticebasedDataStructures2015] here, similar to what TypOS [@allaisTypOSOperatingSystem2022a] is doing?

\newpage

::: {#refs}
:::

<!---
Local Variables:
eval: (progn (olivetti-mode 't) (flyspell-mode 't) (flyspell-buffer)) ;; because it looks better this way!
reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/abstract/bib.bib") ;; add reftex support
End:
-->
