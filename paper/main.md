---
title: 'ExEl: Building an Elaborator Using Extensible Constraints'
date: \today

output: pdf_document

documentclass: acmart
classoption:
  - sigconf
  - screen
numbersections: true
---

# Introduction #  {#sec:introduction}

Statically typed languages allow us to catch large classes of bugs at compile-time by checking the implementation against its type signature.
When the types are provided by the user they can be viewed as a form of specification, constraining the behaviour of the programs.
This comes with the benefit of more static guarantees but with an increased toll on the user to supply more precise information about the program.
Many languages choose to infer types as a result, but another option is to use the information in the types to infer parts of the program.
This idea was aptly worded by Conor McBride as "Write more types and fewer programs." [@ptoopTypeInferenceThought2022; @mcbrideEpigramPracticalProgramming2005, chap. 2.1]
Some examples of this include overloaded functions in Java, implicits in Scala, and type classes in Haskell.

In dependently typed languages like Agda [@norellPracticalProgrammingLanguage2007; @theagdateamAgdaUserManual2023a], Coq [@thecoqdevelopmentteamCoqProofAssistant2022] or Idris [@bradyIdrisGeneralpurposeDependently2013] the types can be much more precise.
This allows us to infer even larger parts of the program from the type.
Examples include implicit arguments in Agda, implicit coercions in Coq, and tactic arguments in Idris.
The inference of parts of the program must not be fully automatic but can also be interactive or partially automatic.
Examples of interactive inference are holes in Agda and proof obligations in Coq, while canonical structures [@mahboubiCanonicalStructuresWorking2013] in Coq and program-synthesis for holes in Haskell [@koppelSearchingEntangledProgram2022] are partially automatic.

Each of these different inference features has its own algoritms and extension points, which often evolved organically over time together with the language, and are often not well isolated from each other.
For example, implicit arguments and instance search in Agda can interact in unexpected ways [@agdausersPerformanceRegressionIssue2018].
Sized types in Agda [@abelExtensionMartinLofType2016] also come with their own solver that often interacts poorly with the regular solver for implicit arguments.
In Coq, canonical structures are notorious for producing unpredictable results yet they were not properly documented for 15 years [@mahboubiCanonicalStructuresWorking2013].
Lean 4 aims to allow the users to develop new surface-level features [@leonardodemouraLeanMetaprogramming2021] using elaboration monads [@mouraLeanTheoremProver2021], somewhat akin to elaborator reflection in Idris [@christiansenElaboratorReflectionExtending2016], but Lean 3 was built in a more conventional way [@demouraLeanTheoremProver2015].
All these bespoke algorithms and their interactions put a toll on the language developer to specify and implement them and on the user to understand them.

The part of the implementation of a dependently typed language that is responsible for type checking user-facing surface syntax and inferring the parts that have been left implicit is known as the *elaborator*.
One common piece of infrastructure used by elaborators are metavariables, also known as "existential variables" [@thecoqdevelopmentteamCoqProofAssistant2022, chap. 2.2.1], which represent as-of-yet unknown parts of the program.
Together with metavariables also comes unification, i.e. the ability to constrain two terms to be equal.
Metavariables and unification are heavily used throughout many elaborators for infering implicit arguments and for general type-checking, making them sensitive towards changes in unification algorithms.
Because of the complexity unification, breaking changes are often discovered only when run against a large existing project on CI, such as the Standard and Cubical libraries for Agda or the `unimath` library for Coq.

To move towards a cleaner and more maintainable model for implementing elaborators, we propose a new architecture for an extensible elaborator for dependently typed languages.
Practically, our architecture allows each feature to be contained within one module, as opposed to being spread around the codebase.
The implementations of the individual features interact with the rest of the elaborator through an API for providing new kinds of constraints and new solvers for these new constraints as well as existing ones.
This allows developers to put all solvers related to one feature in one place, thus fulfilling our goal of modularity.
The design also separates the 'what' the solvers are doing from the 'when', making the separation between different phases of elaboration explicit.
As a result, this allows the developer to reason more easily about exceptions and asynchronicity during elaboration and add new features in a more isolated fashion.

**Contributions**.

* We propose a new design blueprint for implementing an elaborator for a dependently typed language that is extensible with new constraints and new solvers. It supports type classes (Section @sec:case-typeclasses), implicit arguments (Section @sec:case-implicits), implicit coercions, and tactic arguments (Section @sec:coercion-tactics).
* We propose a new view on metavariables as communication channels for the solvers, drawing parallels with asynchronous programming primitives (Section @sec:solvers-implementation).
* We decompose the usual components of an elaborator, like the unifier in Agda, into a suite of solvers which can be extended and interleaved by user-provided plugins (Section @sec:constraints_and_unification).
* Following the blueprint, we present a prototype implementation of a dependently typed language available at [github.com/liesnikov/extensible-elaborator](https://github.com/liesnikov/extensible-elaborator) [^archived-source].


[^archived-source]: Also archived at [doi.org/10.4121/e74fd14a-da79-4686-97e8-143ac5e0858d.v1](https://doi.org/10.4121/e74fd14a-da79-4686-97e8-143ac5e0858d.v1)

# Unification, constraint-based elaboration and design challenges # {#sec:unification_constraint_based_elaboration_and_design_challanges}

Constraints have been an integral part of compilers for strongly typed languages for a long time [@oderskyTypeInferenceConstrained1999].
For example, the implementations of both Haskell [@vytiniotisOutsideInModularType2011] and Agda [@norellPracticalProgrammingLanguage2007 chap. 3] use constraints extensively.
In the former case, they are even reflected and can be manipulated by the user [@orchardHaskellTypeConstraints2010a, chap. 6.10.3; @ghcdevelopmentteamGHCUserGuide2022].
This has proven to be a good design decision for GHC, as is reflected for example in a talk by @peytonjonesTypeInferenceConstraint2019, as well as in a few published sources [@vytiniotisOutsideInModularType2011; @peytonjonesPracticalTypeInference2007].

In the land of dependently typed languages constraints are often used in a much less principled manner.
Agda has a family of constraints that grew organically, currently counting 19 constructors.[^agda-constraints-datatype]
Idris technically has constraints, with the only two constructors being equality constraints for two terms and for two sequences of terms.[^idris-constraints-datatype]
The same holds for Lean.[^lean-constraints-datatype]
These languages either use constraints in a restricted, single-use-case manner -- namely, for unification -- or in an ad-hoc manner.

In this section, we demonstrate why a more methodical approach to constraints results in more robust elaborators across the board.
In particular, we go over three typical challenges that come up when building a compiler for a dependently typed language and the way they are usually solved: the complexity of managing a global state of metavariables for unification (Section @sec:conversion_checking), dealing with different kinds of implicit arguments and their solvers (@sec:implicit-arguments), and user-facing extension points to unification (Section @sec:extending-unification).

[^agda-constraints-datatype]:
[./src/full/Agda/TypeChecking/Monad/Base.hs#L1157-L1191](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Monad/Base.hs#L1157-L1191). Here and henceforth we shorten the links in footnotes to paths in the repository, the source code can be found at [github.com/agda/agda/blob/v2.6.4/](https://github.com/agda/agda/blob/v2.6.4/).


[^idris-constraints-datatype]: [./src/Core/UnifyState.idr](https://github.com/idris-lang/Idris2/blob/e673d05a67b82591131e35ccd50fc234fb9aed85/src/Core/UnifyState.idr) at  
[github.com/idris-lang/Idris2/blob/e673d0](https://github.com/idris-lang/Idris2/blob/e673d05a67b82591131e35ccd50fc234fb9aed85)

[^lean-constraints-datatype]: [./src/Lean/Meta/Match/Basic.lean#L161](https://github.com/leanprover/lean4/blob/0a031fc9bbb43c274bb400f121b13711e803f56c/src/Lean/Meta/Match/Basic.lean#L161) at  
[github.com/leanprover/lean4/blob/0a031f](https://github.com/leanprover/lean4/blob/0a031fc9bbb43c274bb400f121b13711e803f56c/)

## Unification in the presence of meta-variables ## {#sec:conversion_checking}

As mentioned in the introduction, in the process of type-checking we use unification to compare terms.
The unification algorithms are often one of the most complex parts of a type checker, which stems from the desire of compiler writers to implement the most powerful unifier while being limited by the fact that higher-order unification is undecidable in general.
Some of this complexity is unavoidable, but we can manage it better by splitting up the unifier into smaller modular parts.
In practice, this means that one does not have to fit together an always-growing unifier but can instead write different unification rules separately.

As a real-world example, Agda's unification algorithm for solving implicit arguments is spread between about a hundred functions and 2200 lines of code[^conversion-check-agda].
Each of the functions implements part of the "business logic" of the unifier,
but a large part of their implementation is just there to deal with bookkeeping related to metavariables and constraints:

1. They throw and catch exceptions, driving the control flow of the unification.
2. They compute blockers that determine when a postponed constraint should be re-tried.
3. They have special cases for when either or both of the sides equation or its type are metavariables, or for when they are terms whose evaluation is blocked on some metavariables.

Concretely, this code uses functions like `noConstraints`, `dontAssignMetas`, `catchConstraint`, and `patternViolation` which rely on specific behaviour of the constraint solver.
Other functions like `reduceB` and `abortIfBlocked` force the programmer to choose between letting the constraint system handle blockers or doing it manually.
These things are known to be brittle and pose an increased mental overhead when making changes, with a corresponding risk of introducing new bugs.

The unifier is called from many places throughout the type-checker: when checking applications of variables and defined functions, type annotations on lambda expressions, forced `dot' patterns, macro calls, and various primitive operations of Cubical Agda.
At the same time, it is unintuitive and full of intricacies as indicated by multiple comments[^intricate-comments].

We would like the compiler-writer to be able to separate managing constraints and blockers from the actual logic of the comparison function.
If we zoom in on the `compareAtom` function, the core logic can be expressed in about 20 lines[^20lines-compareAtom] of simplified code (ignoring advanced features like sized types, cumulativity, polarity, and forcing). This is precisely what we would like the developer to write.

``` haskell
case (m, n) of
  (Lit l1, Lit l2) | l1 == l2 -> return ()
  (Var i es, Var i' es') | i == i' -> do
      a <- typeOfBV i
      compareElims [] [] a (var i) es es'
  (Con x ci xArgs, Con y _ yArgs) | x == y -> do
      t' <- conType x t
      compareElims t' (Con x ci []) xArgs yArgs
  ...
```

The functions described above are specific to Agda but in other major languages we can find similar problems with unifiers being large pieces of code that are hard to understand.
The sizes of modules with unifiers are as follows: Idris has 1.5kloc[^idris-unifier] of unification code, Lean 1.8kloc[^lean-unifier], and Coq 1.8kloc[^coq-unifier].
For Haskell, which is not a dependently typed language yet, but does have a constraints system [@peytonjonesTypeInferenceConstraint2019], this number is at 2kloc[^ghc-unifier].

[^conversion-check-agda]: [./src/full/Agda/TypeChecking/Conversion.hs](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Conversion.hs)
[^intricate-comments]: [./src/full/Agda/TypeChecking/Conversion.hs##L484-L485](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Conversion.hs#L484-L485), [./src/full/Agda/TypeChecking/Conversion.hs#L541-L549](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Conversion.hs#L541-L549)
[^20lines-compareAtom]:
[./src/full/Agda/TypeChecking/Conversion.hs#L550-L599](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Conversion.hs#L550-L599)


[^idris-unifier]: [./src/Core/Unify.idr](https://github.com/idris-lang/Idris2/blob/102d7ebc18a9e881021ed4b05186cccda5274cbe/src/Core/Unify.idr)
[^lean-unifier]: [./src/Lean/Meta/ExprDefEq.lean](https://github.com/leanprover/lean4/blob/75252d2b85df8cb9231020a556a70f6d736e7ee5/src/Lean/Meta/ExprDefEq.lean)
[^coq-unifier]: [./pretyping/evarconv.mli](https://github.com/coq/coq/blob/b35c06c3ab3ed4911311b4a9428a749658d3eff1/pretyping/evarconv.mli) at [github.com/coq/coq/blob/b35c06](github.com/coq/coq/blob/b35c06c3ab3ed4911311b4a9428a749658d3eff1/)
[^ghc-unifier]:
[./compiler/GHC/Core/Unify.hs](https://gitlab.haskell.org/ghc/ghc/-/blob/b81cd709df8054b8b98ac05d3b9affcee9a8b840/compiler/GHC/Core/Unify.hs) at  
[gitlab.haskell.org/ghc/ghc/-/blob/b81cd709d](https://gitlab.haskell.org/ghc/ghc/-/blob/b81cd709df8054b8b98ac05d3b9affcee9a8b840)

## Type-checking function application in the presence of implicit arguments ## {#sec:implicit-arguments}

During the type-checking of function application, there may be different kinds of implicit arguments to infer, for example, instance arguments, or tactic arguments.
If we start from a simple case of type-checking an application of a function symbol to regular arguments, every next extension needs to be handled as a special case.

Take Agda -- when checking an application of a function with implicit arguments[^agda-insertion-of-implicit-arguments] we already have to carry the information on how the arguments will be resolved (e.g. through instance search) and then create a specific kind of metavariable[^agda-specific-kinds-of-metavariables] [@norellPracticalProgrammingLanguage2007, chap. 3] for each of those cases.

For handling `auto` variables, Idris 2 [@theidristeamIdrisTutorial2021, chap. 13.1] has to essentially inline the search procedure through a chain of elaboration function calls (`checkApp` to `checkAppWith` to `checkAppWith'`) to `makeAutoImplicit`[^idris2-makeautoimplicit-source].
This can accommodate interfaces (or type classes), but one can imagine that if a different kind of implicit was added, like tactic arguments or canonical structures, we would have to inline the search again, requiring a non-trivial modification to the elaboration mechanism.

[^idris2-makeautoimplicit-source]:
[./src/TTImp/Elab/App.idr#L224-L241](https://github.com/idris-lang/Idris2/blob/870bc824371d504a03af937f326216302210a875/src/TTImp/Elab/App.idr#L224-L241)

[^agda-insertion-of-implicit-arguments]: [./src/full/Agda/TypeChecking/Implicit.hs#L96-L128](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Implicit.hs#L96-L128)

[^agda-specific-kinds-of-metavariables]: [./src/full/Agda/TypeChecking/Implicit.hs#L130-L149](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Implicit.hs#L130-L149)

While the codebases above show that it is certainly possible to extend languages with new features if the language was not written with extensibility in mind, this can lead to rather ad hoc solutions.

## Extending unification ## {#sec:extending-unification}

While writing a unifier is hard enough as it is, at times the developers might want to give their users the ability to extend the unification procedure.

Canonical structures [@saibiOutilsGeneriquesModelisation1999; @mahboubiCanonicalStructuresWorking2013] are one example as they are in the overlap between type classes and unification hints.
Adding them to a language that does not support them requires an extension of the unification algorithm with a rule that says that projection from a canonical structure is injective [@mahboubiCanonicalStructuresWorking2013, eq. 1].

One could also provide means to do so manually in a more general case, by allowing users to declare certain symbols as injective.
This is one of the features requested by the Agda users [@agdausersInjectiveUnificationPragma2023].

Another example of user-extensible unification can be found in extensions with rules for associativity and commutativity, as described in the thesis by @holtenDependentTypeCheckingModulo2023.
It required changes to 2000 lines of code[^holten-source].
We would like to make changes such as this more feasible.

[^holten-source]: [github.com/LHolten/agda-commassoc/tree/defenitional-commutativity](https://github.com/LHolten/agda-commassoc/tree/defenitional-commutativity) (sic)

## Summary of the issues ## {#sec:summary-of-the-issues}

The examples above show that when building a dependently typed language the core might be perfectly elegant and simple, but the features that appear on top of it complicate the design.
While the code in existing implementations often relies on constraints, the design at large does not put them at the centre of the picture and instead views them primarily as a gadget.
For example, Agda's constraint solver[^agda-constraint-solver-source] relies on the type-checker to call it at the point where it is needed and has to be carefully engineered to work with the rest of the codebase.


# A blueprint for extensible elaborators ## {#sec:what-is-our-design-bringing-into-the-picture}

Our idea for a new design is to shift focus more towards the constraints themselves:

1. We give an API for raising constraints that can be called by the type-checker, essentially creating an "ask" to be fulfilled by the solvers.
This is similar to the idea of mapping object-language unification variables to host-language ones as done by @guidiImplementingTypeTheory2017, the view of the "asks" as a general effect [@bauerEqualityCheckingGeneral2020, chap. 4.4], or the communication between actors [@allaisTypOSOperatingSystem2022a].

2. To make the language more modular, we make constraints an extensible data type and give an API to define new solvers with the ability to specify what kinds of constraints they can solve. Since we're working in Haskell we encode constraints in the style of Data types à la carte [@swierstraDataTypesCarte2008].

In the examples in this paper, we follow the bidirectional style of type-checking.
In practice, however, the design decisions are agnostic of the underlying system, as long as it adheres to the principle of stating the requirements on terms in terms of raising a constraint and not by, say, pattern-matching on a concrete term representation.

\begin{figure*}
  \center

  \includegraphics[width=\textwidth]{architecture-diagram.png}

  \caption{Architecture diagram}
  \label{architecture-figure}
\end{figure*}


From a birds-eye view, the architecture looks as depicted in Figure \ref{architecture-figure}.
Its main characteristics are the separation of plugins/unification into independent pieces and a clear boundary between solver dispatcher and the rest of the system.

Let us walk through each part of the architecture in more detail.
The type-checking begins by initializing the state and traversing the syntax.
The traversal raises the constraints, and for the moment, the constraints are simply stored.
As soon as we finish the traversal of a block (one declaration in our case), the solver dispatcher is called.
It traverses the set of constraints, and tries to solve each active constraint by making calls to different plugins.
Each plugin, whether user-supplied (`Plugin A`) or provided by us (`unification`) consists of a handler and a solver.
The handler determines if the plugin can potentially solve a constraint, if so, the dispatcher runs the corresponding solver.

All components have some read access to the state, including handlers which might for example verify that there are no extra constraints on the metavariable.
For the write access: syntax traversal writes new metavariables to the state and elaborated definitions; the solver dispatcher writes updated meta-information; solvers write solutions to the metavariables and can raise new constraints.

For the moment we need to recompile the project to include new plugins.
This is not necessity and a system that dynamically loads plugins is possible to implement in a way that is similar to GHC Plugins[^plugins-link] or Accelerate[^acclerate-link] [@mcdonellTypesafeRuntimeCode2015].

[^agda-constraint-solver-source]:
[src/full/Agda/TypeChecking/Constraints.hs#L247-L298](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Constraints.hs#L247-L298)

[^plugins-link]: [mpickering.github.io/plugins.html](https://mpickering.github.io/plugins.html)

[^acclerate-link]: [github.com/tmcdonell/accelerate-llvm/blob/master/accelerate-llvm-native/src/Data/Array/Accelerate/LLVM/Native/Link/Runtime.hs](https://github.com/tmcdonell/accelerate-llvm/blob/master/accelerate-llvm-native/src/Data/Array/Accelerate/LLVM/Native/Link/Runtime.hs#L40)

# Dependently typed calculus and bidirectional typing # {#sec:bidirectional}

Now that we have described our architecture, we will demonstrate its design through a practical implementation.
First, in this section, we describe the core of the type system we implement. 
The following sections will then describe the constraints and their solvers.
We take pi-forall [@weirichImplementingDependentTypes2022] as a basis for the system and extend it with metavariables in the core syntax and implicit arguments in the surface syntax.
However, for all other purposes, we leave the core rules intact and, therefore, the core calculus too.

## Basic language and rules ##

This is a dependently typed calculus that includes Pi, Sigma and indexed inductive types.

Here is the internal syntax term data type, apart from service constructor omissions, like a placeholder `TRUSTME`.

```haskell
data Term =
  -- type of types Type
    Type
  -- variables x
  | Var TName
  -- abstraction \x. a
  | Lam (Bind TName Term)
  -- application a b
  | App Term Arg
  -- function type (x : A) -> B
  | Pi Type (Bind TName Type)
  -- Sigma-type { x : A | B }
  | Sigma Term (Bind TName Term)
  | Prod Term Term
  | LetPair Term (Bind (TName, TName) Term)
  -- Equality type  a = b
  | TyEq Term Term
  | Refl
  | Subst Term Term
  | Contra Term
  -- inductive datatypes
  | TCon TCName [Arg] -- types (fully applied)
  | DCon DCName [Arg] -- terms (fully applied)
  | Case Term [Match]
    -- metavariables
  | MetaVar MetaClosure
```

Equality is not defined as a regular inductive type but is instead built-in.
The user has access to the type and term constructor, but not the ability to pattern-match on it.
Instead, the language provides a `subst` primitive of type `(A x) -> (x=y) -> A y` and `contra` that takes an equality of two different constructors of an inductive type and produces an element of any type.

On top of the above, the language includes indexed inductive datatypes and case-constructs for their elimination.
Indexed inductive datatypes are encoded as parameterised datatypes with an equality argument constraining the index, also known as "Henry Ford" equality [@chapmanGentleArtLevitation2010a].

As for metavariables `MetaVar`: as mentioned in the introduction, they are placeholders in the syntax tree (AST) that are produced in the process of elaboration.
Metavariables do not appear in the surface syntax as they are not created by the user.
In this paper we implement metavariables in the contextual style, as described by @abelHigherOrderDynamicPattern2011, therefore they are paired with a closure of type `MetaClosure`.

## Syntax traversal ##

We implement the core of the elaborator as a bidirectional syntax traversal, raising a constraint every time we need to assert something about the type.

This includes the expected use of unification constraints, for example the case when we enter checking mode with a term whose type can be inferred:

```haskell
checkType tm ty = do
  (etm, ty') <- inferType tm
  constrainEquality ty' ty I.Type
  return etm
```

It also includes any time we want to decompose the type provided in checking mode we pose a constraint that guarantees the type being in a specific shape:

```haskell
checkType (S.Lam lam) ty = do
  mtyA <- createMetaTerm
  mtx <- createUnknownVar
  -- extend the context with a new variable signature
  mtyB <- extendCtx (I.TypeSig (I.Sig mtx mtyA))
                    (createMetaTerm)
  -- metaPi is the shape we want ty to be in
  let metaPi = I.Pi mtyA (bind mtx mtyB)

  constrainEquality ty metaPi I.Type
  -- rest of the traversal can now use mtyA and mbnd
  ...
```

At certain points, we have to raise a constraint which has an associated continuation.
For example, when checking the type of a data constructor, the part of the program that comes as an argument to `constrainTConAndFreeze` will be suspended (or "blocked") until the meta is solved with something of the shape `TCon _ _`.

```haskell
checkType t@(S.DCon c args) ty = do
  elabpromise <- createMetaTerm
  constrainTConAndFreeze ty $ do
    mty <- substMetas ty
    case mty of
      (I.TCon tname params) -> do
        ...
      _ -> error "impossible"
```

We add one more case to the elaborator for implicit arguments of any kind, as will be described in more detail in Section @sec:case-implicits.

```haskell
checkType (Implicit) ty = do
  m <- createMetaTerm
  raiseConstraint $ FillInTheMeta m ty
  return m
```

As we see, the syntax traversal does not need to know anything about how implicit arguments are resolved.
The only thing we require is that the elaboration of the argument is called with the type information available.
This corresponds to how, in bidirectional typing, function application is done in the inference mode but the arguments are processed in checking mode.

``` haskell
inferType (App t1 t2) = do
  (et1, Pi tyA tyB) <- inferType t1
  et2 <- checkType t2 tyA
  return (App et1 et2, subst tyB et2)
```




# Constraints and unification # {#sec:constraints_and_unification}

In Section @sec:bidirectional we described the syntax traversal part of the elaborator, which generates the constraints.
In this section, we will go over the constraint datatype needed for the base language, how unification is implemented and how we can extend the unification procedure.

## Base constraints

The datatype of constraints is open, which means the user can write a plugin to extend it.
However, we offer a few constraints out of the box to be able to type-check the base language.
For the base language, it suffices to have the following:

* A constraint that enforces equality of two terms of a given type
  ``` haskell
  -- two terms given should be equal
  data EqualityConstraint e =
       EqualityConstraint Term Term Type MetaVarId
  ```
  Here, `MetaVarId` refers to the anti-unification variable (see Section @sec:solvers-implementation).
* A constraint that ensures that a metavariable is resolved eventually:
  ```haskell
  -- this terms has to be filled in
  data FillInTheTerm e =
       FillInTheTerm Term (Maybe Type)
  ```
* Lastly, a constraint which ensures that a term is a type constructor:
  ```haskell
  -- the term passed to the constraint
  -- should be a type constructor
  data TypeConstructorConstraint e =
       TypeConstructorConstraint Type
  ```

The type-checker raises constraints and supplies the necessary information in the process, but it is agnostic of how they will be solved.
In all of the examples above, the parameter `e` is spurious.
The need for this parameter comes from the technique we use to encode open datatypes, and other constraint types can use it to encode recursive occurrences of the constraints.

## Interface of the solvers ## {#sec:solvers-interface}

On the solver side, we provide a suite[^list-solvers] of solvers for unification that handle different cases of the problem.
The most basic plugin is the `syntactic` plugin which resolves equality constraints where both sides are syntactically equal.

``` haskell
-- solves syntactically equal terms
syntacticHandler :: (EqualityConstraint :<: c)
                 => HandlerType c
syntacticHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    -- check for alpha-equality
    Just (EqualityConstraint t1 t2 ty _) ->
      return $ aeq t1 t2
    Nothing ->
      return False
syntacticSolver :: (EqualityConstraint :<: c)
                => SolverType Bool
syntactic :: Plugin
syntactic  = Plugin { solver  = syntacticSolver
                    , handler = syntacticHandler
                    ...
                    }
```


We first define the class of constraints that will be handled by the solver via providing a "handler" -- a function that decides whether a given solver has to fire.[^code-note]
In this case, this amounts to checking that the constraint given is indeed an `EqualityConstraint` and that the two terms given to it are syntactically equal.
Then we define the solver itself.
In this case does not have to do anything except return `True` to indicate that the constraint is solved.
This is because it shall only fire once it has been cleared to do so by the handler and the equality has already been checked.
Finally, we register the solver by declaring it using a plugin interface.

The reason for the separation between a decision procedure and the execution of the solver is to ensure separation between a potentially slow, effectful solving and fast read-only decision-making in the handler.
We opt for this division since handlers will be run on many constraints that do not fit them, therefore any write effects would have to be rolled back.
Solvers, on the other hand, should only fire in cases when we can reasonably hope that the constraint will be solved and the effects will not have to be rolled back.

In a similar fashion, we define `leftMetaSolver` and `rightMetaSolver`, which only work on problems where one of the sides is a metavariable.
Here the job of the solver is not as trivial -- it has to check that the type of the other side indeed matches the needed one and then register the instantiation of the metavariable in the state.

Since a single constraint can often be handled by multiple solvers, we can provide priority preferences, using the `pre` and `suc` fields of the plugin interface.
They are used to indicate whether the currently defined plugin should run before or after, respectively, which other plugins.
At the time of running the compiler, these preferences are loaded into a big pre-order relation for all the plugins, which is then linearised and used to guide the solving procedure.

```haskell
rightMetaPlugin :: (EqualityConstraint :<: cs)
                => Plugin cs
rightMetaPlugin =
  Plugin { handler = rightMetaHandler
         , solver  = rightMetaSolver
         , symbol  = rightMetaSymbol
         , pre = []
         , suc = [leftMetaSymbol]
         }
```

[^list-solvers]: In the prototype we implement a subset of all unification rules, specifically: `identityPlugin`, `propagateMetasEqPlugin`, `reduceLeftPlugin`, `reduceRightPlugin`, `leftMetaPlugin`, `rightMetaPlugin`, `typeConstructorPlugin`, `typeConstructorWithMetasPlugin`, `piEqInjectivityPlugin`, `tyEqInjectivityPlugin`, `consInjectivityPlugin`, `typeInjectivityPlugin`, `unificationStartMarker`, and `unificationEndMarker`.

[^code-note]: The code shown above and in the rest of the paper is close to the actual implementation, but has been simplified for presentation purposes. `HandlerType a` and `SolverType a` both morally correspond to `(ConstraintF cs) -> SolverMonad Bool`.


## Implementation of the solvers and unification details ## {#sec:solvers-implementation}

We implement a system close to the one described by @abelHigherOrderDynamicPattern2011.
We modularise the implementation by mapping every function call in the simplification procedure to a raised constraint and every simplification rule to a separate solver.
For example, the "decomposition of functions" [@abelHigherOrderDynamicPattern2011, fig. 2] rule is translated to the following implementation.

```haskell
piEqInjectivityHandler :: (EqualityConstraint :<: cs)
                       => HandlerType cs
piEqInjectivityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint pi1 pi2 _ _) ->
      case (pi1, pi2) of
        (I.Pi _ _ _, I.Pi _ _ _) -> return True
        _ -> return False
    _ -> return False
```

The handler is simply checking that both sides of the equality are indeed Pi-types, and in case either of the matches fails, it will be reported and the solver will not be fired.
The `match` function above comes from the open datatype of constraints [@swierstraDataTypesCarte2008, sec. 5], checking if `constr` can be projected from `cs` to `EqualityConstraint`.

Now let us take a look at the solver.
The rule we are implementing here states that two $\Pi$-types can only be equal if both the types of the domains (`a1`, `a2`) and the co-domains (`b1`, `b2`) are equal.

```haskell
piEqInjectivitySolver :: (EqualityConstraint :<: cs)
                      => SolverType cs
piEqInjectivitySolver constr = do
  let (Just (EqualityConstraint (I.Pi a1 b1)
                                (I.Pi a2 b2) _ m)) =
        match @EqualityConstraint constr
  ma <- constrainEquality a1 a2 I.Type
  (x, tyB1) <- unbind b1
  (y, tyB2') <- unbind b2
  let tyB2 = subst y (I.Var x) tyB2'
      mat  = I.identityClosure ma
  mb <- extendCtx (I.TypeSig (I.Sig x e1 mat)) $
                  constrainEquality tyB1 tyB2 I.Type
  let mbt = bind x $ I.identityClosure mb
  solveMeta m (I.Pi mat mbt)
  return True
```

Following pi-forall, we use the unbound-generics[^unbound-link] library to deal with the names and binders (`unbind`, `subst` functions).
As it will fire after a handler returns `True`, we can assume the pattern-matches will not fail.

First, we constrain the equality of the domain of the Pi-type: `a1` and `a2`.
The seemingly spurious metavariable `ma` returned from this call serves as an anti-unification [@pfenningUnificationAntiunificationCalculus1991] communication channel.
Every time an equality constraint is created we return a metavariable that stands for the unified term.
This metavariable is used for unification problems that are created in the extended contexts -- in this case second argument of the Pi-type, but also when solving equalities concerning two data constructors.
We do this to tackle the "spine problem" [@victorlopezjuanPracticalHeterogeneousUnification2021, sec. 1.4] -- as we operate according to the "well-typed modulo constraints" principle, essentially providing a placeholder that is guaranteed to preserve well-typedness in the extended context.[^anti-unification-note]
Finally, `ma` has to be applied to a closure, which will keep track of the delayed substitution.

Then we can constrain the co-domains of Pi-types in an extended context.
In case one of the solvers the constraints created in the extended context might need to know the exact shape of `ma`, we can block on the metavariable later, freezing the rest of the problem until it is instantiated.

As for the actual unification steps, we implement them in a similar fashion to the simplification procedure.
For example, `leftMetaSolver` below handles the case where the left-hand side of an equality constraint is an unsolved meta:

```haskell
leftMetaSolver :: (EqualityConstraint :<: cs)
               => SolverType cs
leftMetaSolver constr = do
  let (Just (EqualityConstraint t1 t2 _ m)) =
        match @EqualityConstraint constr
      (MetaVar (MetaVarClosure m1 c1)) = t1
  mt2 <- occursCheck m1 t2
  case mt2 of
    -- indicates a failure in occurs-check
    Left e -> return False
    -- indicates a passed occurs-check
    Right t2 ->
      case (invertClosureOn c1 (freeVarList t2)) of
        Just s -> do
          let st2 = substs s t2
          -- apply the subsitution
          solveMeta m1 st2
          -- instantiate the anti-unification variable
          solveMeta m st2
          return True
        Nothing -> return False
```

Once the occurs-check returns and if it was successful, we apply the inverted closure to the right-hand side of the equality.

By splitting up the rules into individual, simple solvers we can compartmentalise the complexity of the unifier, making sure that each rule is as decoupled from the others as possible.
This does not deteriorate the properties of the system but does not help to enforce them either.
We talk more about the challenge of proving correctness in Section @sec:limitations.

[^unbound-link]: [hackage.haskell.org/package/unbound-generics-0.4.3](https://hackage.haskell.org/package/unbound-generics-0.4.3)

[^anti-unification-note]: $\text{Tog}^{+}$ [@victorlopezjuanTog2020; @victorlopezjuanPracticalHeterogeneousUnification2021] focuses on extending the unification algorithm for the case where two sides of equality might not be of the same type, which is also a problem relevant for us. Their main argument against the usage of anti-unification in Agda provided there is that it is bug-prone. We think that in Agda the problems were stemming from the fact that anti-unification was implemented separately from unification, in which case it is indeed hard to keep the two in sync. There is no such duplication in our case since unification and anti-unification are one.

## Extending unification ##

Now that unification is implemented let us create a simple plugin that makes certain symbols declared by the user injective [@agdausersInjectiveUnificationPragma2023].
The actual implementation is relatively simple and is not dissimilar to the Pi-injectivity solver we showed above.

```haskell
userInjectivitySolver :: (EqualityConstraint :<: cs)
                      => SolverType cs
userInjectivitySolver constr = do
  let (Just EqualityConstraint
              (I.App (I.Var f) a)
              (I.App (I.Var g) b) _ m)) =
        match @EqualityConstraint constr
  if f == g
  then do
    ifM (queryInjectiveDeclarations f)
        (do
           ms <- constrainEquality a b I.Type
           solveMeta m ms
           return True)
        (return False)
  else return False
```

where `queryInjectiveDeclarations` simply scans the available declarations for a marker that `f` has been declared injective.

The only big thing left is to make sure that this solver fires at the right time.
This can only conflict with the "decomposition of neutrals" rule, so we indicate to the solver dispatcher that our plugin should run before it:

```haskell
userInjectivityPlugin :: (EqualityConstraint :<: cs)
                      => Plugin cs
userInjectivityPlugin =
  Plugin { ...
         , solver = userInjectivitySolver
         , symbol = PluginId $ "userInjectivity"
         , pre = [ unifyNeutralsDecomposition
                 , unificationEndMarkerSymbol]
         , suc = [unificationStartMarkerSymbol]
         }
```

This modification does not alter the core of the language.

# Case studies # {#sec:casestudies}

Once we implement basic elaboration and unification we can extend the language.
This is where we make use of the fact that the constraints datatype is open.

We saw before in Section @sec:implicit-arguments that conventional designs require separate handling of different kinds of implicit variables.
To simplify the design we would like to uniformly dispatch a search for the solution, which would be handled by a fitting solver.
We can achieve this by communicating the kind of the meta through its type in the second argument of `FillInTheMeta m ty`.
The solvers then match on the shape of the type of the metavariable and handle it in a case-specific manner: instance-search for type classes, tactic execution for a tactic argument, or waiting for regular unification to solve the metavariable for regular implicit arguments.

In this section, we will describe the implementation details of regular implicit arguments (Section @sec:case-implicits) and the implementation of type classes added on top of the implicit arguments (Section @sec:case-typeclasses).
And finally (Section @sec:coercion-tactics) we sketch the implementation of coercive subtyping and tactic arguments.

## Implicit arguments ## {#sec:case-implicits}

As we mentioned, the rule for `Implicit` had to be added to the syntax traversal part of the elaborator.
In fact, we require not one but two modifications that lie outside of the solvers-constraints part of the system.
The first one is, indeed, the addition of a separate case in the syntax traversal, however contained.
The second one lies in the purely syntactical part of the compiler:
we need the pre-processor to insert the placeholder terms in the surface syntax.

In particular, we need to desugar declarations of functions in the following way.
For any declaration of a function with some implicit arguments, we wrap the type of each implicit argument with an `Implicit`:
```
def f : {A : Type} -> {a : A} -> B a -> C
```
becomes
```
def f : (A : Implicit Type)
     -> (a : Implicit (deImp A))
     -> (b : B (deImp a)) -> C
```

Then, for each function call we insert the corresponding number of implicit arguments, for example transforming `f b1` to `f _ _ b1`.

For basic usage of implicit arguments this suffices -- as soon as we have the placeholders in the surface syntax we create the constraints in the `Implicit` case of the syntax traversal.
The only solver that is needed in this case is a trivial one that checks that the metavariable has been instantiated in the end.
The actual solving in this case is done by the unifier itself, and
the constraint simply serves as a way to guarantee that all implicits are eventually instantiated.

```haskell
fillInImplicitSymbol :: PluginId

fillInImplicitHandler :: (FillInImplicit :<: cs)
                      => HandlerType cs
fillInImplicitHandler constr = do
  let ficm = match @FillInImplicit constr
  case ficm of
    Just (FillInImplicit term ty) -> do
      case term of
        MetaVar (MetaVarClosure mid _) ->
          isMetaSolved mid
        _ -> return False
    Nothing -> return False

fillInImplicitPlugin :: (FillInImplicit :<: cs)
                     => Plugin cs
fillInImplicitPlugin = Plugin {
    solver = fillInImplicitSolver,
  , handler = fillInImplicitHandler
  , symbol = fillInImplicitSymbol
  , suc = []
  , pre = []
  }
```

There is a design choice to be made in the implementation of `Implicit A` and `deImp`.
One option is to turn them into a constructor and a projection of a record type, and the other is to make them computationally equivalent to `id`.
In the former case, we need to manually unwrap them both in the pre-processor and during the constraint-solving, but since the head symbol is distinct we can guarantee that other solvers will not match on it, unless explicitly instructed to.
In the latter case, one has to be cautious of the order in which the solvers are activated, particularly in the case of different search procedures, should they be implemented.
However, in the example above it does not make a difference.

A limitation of this scheme is that we can only support functions with "obvious" implicit arguments -- i.e. those that appear syntactically in the declaration of the function.
This is due to the fact that insertion of metavariables happens before any type information is available.
For this reason Haskell-like impredicativity [@serranoQuickLookImpredicativity2020a] in type inference cannot be supported.
If a more comprehensive support for implicit arguments is desired, our system could be extended with support for first-class implicits [@kovacsElaborationFirstclassImplicit2020].

## Type classes ## {#sec:case-typeclasses}

Next, let us implement a plugin that adds support for type classes by means of instance arguments [@DevrieseP11-1].
As in the case for implicit arguments in general, we rely again on a pre-processor to insert placeholder arguments of type `Instance a` for each instance argument of type `a`.

Let us start by going through an example of the elaboration process for a simple term.
For type classes we need a few declarations, listed below in Agda-like syntax:

```
plus  :  {A : Type} -> {{PlusOperation A}}
     -> (a : A) -> (b : A) -> A

instance BoolPlus : PlusOperation Bool where
  plus = orb
```

And the exemplary term itself is:

```
m = plus True False
```


1. First, the pre-processor eliminates the implicits and type class arguments.
   We end up with the following declarations:
   ```
   plus : (impA : Implicit Type)
       -> Instance PlusOperation (deImp impA)
       -> (a : deImp impA) -> (b :  deImp impA)
       -> deImp impA
   
   instanceBoolPlus : InstanceT PlusOperation Bool
   instanceBoolPlus =
     InstanceC (TypeClassC (Plus orb))
   
   m = plus _ _ True False
   ```
   Turning the declaration `instance BoolPlus` into a usage of the constructor `InstanceC` is precisely the part we need the pre-processor to do.
2. Next, we go into the elaboration of `m`.
   The elaborator applies `inferType (App t1 t2)` rule four times and `checkType (Implicit) ty` twice on the two placeholders.
   The output of the elaborator is
   ```
   m = plus ?_1 ?_2 True False
   ```
   The state of the elaborator now contains four constraints:
   ```
   C1: FillInTheTerm ?_1 (Implicit Type)
   C2: FillInTheTerm ?_2 (InstanceT PlusOperation
                                    (deImp ?_1))
   C3: EqualityConstraint (deImp ?_1) Bool Type
   C4: EqualityConstraint (deImp ?_1) Bool Type
   ```

   The first two correspond to implicit arguments, while
   the latter two are unification problems rendered into constraints.

3. Now we step into the constraint-solving world.
   First, the unifier solves the latter two constraints, instantiating `?_1` to `Implicit Bool`.
   `C1` is then discarded as solved since `?_1` is already instantiated to `Implicit Bool`.
   Next, the type class resolution launches a search for the instance of type `Instance PlusOperation Bool`.

4. This is where the type class plugin can take over.
   It transforms `C2: FillInTheTerm ?_2 (InstanceT PlusOperation Bool)` to `C5: InstanceSearch PlusOperation Bool ?_2`.
   `C5` then gets matched by the search plugin for concrete instances, simply weeding through available declarations, looking for something of the shape `InstanceT PlusOperation Bool`.
   Such a declaration exists indeed and we can instantiate `?_2` to `instanceBoolPlus`.

Now let us take a look at the plugin for the constraint system.
It is contained in a single file [`./exel/src/Plugins/Typeclasses.hs`](https://github.com/liesnikov/extensible-elaborator/blob/elaborator-experiments/exel/src/Plugins/Typeclasses.hs).
In it, we define a new constraint type `InstanceSearch`, a solver that transforms constraints of the shape `FillInTheType ? (InstanceT _)` to the new constraint, and finally, a solver for instance search problems.
The reason to transform the original constraint to the new type is to ensure that no other solver will make an attempt at this problem, therefore modulating interactions with other plugins.

Finally, the implementation of search for concrete instances is quite simple:
```haskell
instanceConcreteSolver constr = do
  let (Just (InstanceSearch tcn ty m)) =
        match @InstanceSearch constr
  alldecls <- collectInstancesOf tcn <$> SA.getDecls
  sty <- SA.substAllMetas ty
  case Map.lookup sty alldecls of
    Just i -> do
      SA.solveMeta m (I.Var i)
      return True
    Nothing -> return False
```


We conjecture that implementation of canonical structures [@mahboubiCanonicalStructuresWorking2013] would be relatively simple in such a system due to the openness of both unification procedure and instance search.

## Tactic arguments and coercions  ## {#sec:coercion-tactics}

In a similar fashion to the transformation of the `FillInImplicit` constraints to `InstanceSearch`, we can implement plugins for tactic arguments and coercive subtyping.

The former would be quite similar to what we saw in the previous section  @sec:case-typeclasses, except we would have to resolve `FillInImplicit` to a `RunTactic` constraint (or run the tactic directly).
The question of how to actually run the tactics is independent of the constraint machinery, noting that we allow for running arbitrary type-checking actions during solving of constraints.

Coercive subtyping is of a slightly different nature.
First, it would be the most reliant on a pre-processor out of all of the examples described above, since coercions could potentially be inserted around each function argument, and potentially also around heads of applications and types of abstractions [@tassiBiDirectionalRefinementAlgorithm2012].
However, blindly inserting placeholders for coercions in every position leads to constraints that cannot be resolved locally.
Consider the following example:

```
f : (A : Type) -> A -> A
arg : ArgTy

t : ExpTy
t = f _ arg
```

This declaration would desugar to

```
t = coerce _ (f _ (coerce _ arg))
```

which gives rise to two constraints, where `Coercion A B` stands for the evidence of `A` being a subtype of `B`:

```
C1 : Coercion ArgTy ?1
C3 : Coercion ?1 ExpTy
```

Clearly, we can not solve the first constraint in isolation and need to consider all coercion constraints related to a particular type at the same time.
Hence this feature would be anti-modular in the sense that in order to side-step the conflict between inference and coercions the solver would need to look at the whole graph of subtyping constraints at the same time.
While we can accommodate such solvers, due to solvers having write access to the state, it becomes much harder to modulate interactions between different plugins.
It also comes with a potential performance penalty, which we describe further in Section @sec:limitations.

# Limitations # {#sec:limitations}

While our design offers a lot of flexibility, it does not solve every problem.
In this section, we describe a few examples of extensions that do not quite fit in this framework as well as more general limitations.

## Handling of meta-variables outside of definition sites ##

After we elaborate a definition, there can still be unsolved metavariables in it.
This presents us with a design choice.
The first option is to generalize the definition over the metavariables and instantiate them per-use site, essentially considering the metavariables as additional (implicit) arguments to the definition.
The second option is to leave them up to be solved later, which might make elaboration less predictable since now the use sites can influence whether a particular definition type-checks.
The third option is to freeze the metavariables by considering them to be postulates, and report them as an error at the end of type checking.

In particular, the second option allows us to incorporate more involved inference algorithms into the system.
For example, if we were to implement an erasure inference algorithm as described by @tejiscakDependentlyTypedCalculus2020, we would have to create metavariable annotations (described as "evars" in the paper) that can be instantiated beyond the definition site.
The downside is that the meaning of a definition can then change depending on where they are used, making the outcome of elaboration depend on the file as a whole rather than the definition itself.

## The language is only as extensible, as the syntax traversal is ##

Extensibility via constraints allows for a flexible user-specified control flow as soon as we step into the constraints world.
However, not all features can be supported purely by adding constraints and solvers.
For example, consider the following simplified lambda-function type-checking function[^agda-lambda-tc-source] from Agda:

``` haskell
checkLambda' cmp b xps typ body target = do
  TelV tel btyp <- telViewUpTo numbinds target
  if size tel < numbinds || numbinds /= 1
    then dontUseTargetType
    else useTargetType tel btyp
```

Here Agda steps away from the bidirectional discipline and infers a (lambda) function if the target type is not fully known.
In order to add such a rule to our implementation, we would have to update syntax traversal to be more flexible in where it allows lambda expressions to appear.
The solution, in this case, is effectively to replicate what Agda is doing by implementing each type-checking rule in inference mode, essentially factoring out `dontUseTargetType` in Agda's code snippet above.

Likewise, implementing the commutativity and associativity unifier plugins from @holtenDependentTypeCheckingModulo2023 requires modifications in the core language, since we two terms that are equal during elaboration have to equal during core type-checking too.

[^agda-lambda-tc-source]: [./src/full/Agda/TypeChecking/Rules/Term.hs#L430-L518](https://github.com/agda/agda/blob/v2.6.4/src/full/Agda/TypeChecking/Rules/Term.hs#L430-L518)

## Lack of backtracking ##

We do not implement any backtracking in the solver dispatcher as it is now.
This means that every step taken is committing to a specific choice to how to solve the constraint, which can be a limitation in cases where one would like to have backtracking -- for example, Agda's instance arguments search (with `--overlapping-instances` flag) [@theagdateamAgdaUserManual2023a, chap. 3.18], as well as type classes in Lean [@selsamTabledTypeclassResolution2020].

Backtracking in principle could be achieved by tracking changes to the state of the elaborator and the production graph for constraints, but such a system would be rather awkward.
Alternatively, controlled backtracking can be implemented within one solver, removing the extension points, but allowing arbitrary control flow within the algorithm.

## Reliance on a pre-processor ##

This work crucially relies on a pre-processor of some kind, be it macro expansion or some other way to extend the parser with custom desugaring rules.
In particular, in order to implement n-ary implicit arguments correctly and easily we need the pre-processor to expand them to the right arity, similar to Matita [@tassiBiDirectionalRefinementAlgorithm2012, chap. 5] and others [@serranoQuickLookImpredicativity2020a; @kovacsElaborationFirstclassImplicit2020].

## Eager reduction and performance ##

As the pre-processing step inserts a fair number of wrappers and un-wrappers for all implicits and even more for coercions, we can expect a performance penalty due to larger terms and spurious computation steps.
In particular, we expect this to be a major concern for coercive subtyping.

As one possible mitigation, we suggest a discipline where constraint solvers latch on to non-reduced types and terms in constraints.
With this discipline, we can borrow a trick from the implementation of Coq, where the wrappers and unwrappers are identity functions and `coerce f t` computes to `f t`.
This also means that constraints can/have to match on unreduced types in the e.g. `FillInTheTherm`.

In fact, we already use this trick to an extent for a different reason -- since the calculus allows only fully applied type constructors, we have to wrap each type class constructor in a lambda-abstraction for it to appear as an argument to `TypeClassT typeClassName argType`.
Or, concretely, we have to define and use `PlusOperation' = \A . PlusOperation A` in place of `PlusOperation` in the elaboration example in Section @sec:case-typeclasses.

## Proving correctness ##

As soon as we allow the users to implement their own solvers, there is little we can say about the correctness of the system as a whole without imposing proof obligations on the plugin writers.
This is simply a consequence of the fact that we do not invent a new unification algorithm, but rather provide means of easier implementation for it.
We conjecture that our system satisfies the solution and type preservation properties -- Theorems 2 and 3 from @abelHigherOrderDynamicPattern2011, respectively -- assuming that each solver on its own satisfies these properties.
For termination the situation is similar -- we conjecture that if every solver in the system "reduces the weight" of the unification problem, in terms of Theorem 1 by @abelHigherOrderDynamicPattern2011, we can guarantee termination of the solving process as a whole.

# Related work # {#sec:related_work}

We are certainly not the first ones to try to tackle the extensibility of a language implementation.
Dependently typed language implementations usually consist of at least four parts:
parser, elaborator, core type-checker, and code generation backend.
The code generation part is currently irrelevant to our interests, since for a language to be specified usually means for specification of the core, anything that happens after the core does not extend the language, but rather tries to preserve its semantics in some form.
Therefore we're left with three parts: parser, elaborator, and core type-checker.

We see parser or syntax extensibility as a necessary part of an extensible language.
This problem has been studied extensively in the past and has a multitude of existing solutions.
Macros are one of them and are used heavily in various forms in many established languages [@thecoqdevelopmentteamCoqProofAssistant2022; @theagdateamAgdaUserManual2023a; @ullrichNotationsHygienicMacro2020] and can be powerful enough to build a whole language around [@changDependentTypeSystems2019].

Core extensibility, on the other hand, appears to be a problem with too many degrees of freedom.
Andromeda [@bauerDesignImplementationAndromeda2018; @bauerEqualityCheckingGeneral2020] made an attempt at extensible definitional equality but is quite far from a usable dependently typed language.
Agda's philosophy allows developers to experiment with the core but also results in a larger amount of unexpected behaviours.
In general, modifications of the core rules will result in fundamental changes in the type theory, which can break plenty of important properties like soundness or subject reduction.

This leaves us with the question of the extensibility of an elaborator.
We will make a division here between syntax traversals, constraint solving and all other features.
The syntax traversal part of the elaborator is relatively stable and commonly implemented following a (roughly) bidirectional discipline[@norellPracticalProgrammingLanguage2007; @tassiBiDirectionalRefinementAlgorithm2012; @ferreiraBidirectionalElaborationDependently2014], so there seems little reason to make it extensible.

GHC has a plugin system that allows users to dynamically add custom constraint solvers, but the type of constraints itself is not extensible[^ghc-note] [@peytonjonesTypeInferenceConstraint2019; @vytiniotisOutsideInModularType2011; @peytonjonesPracticalTypeInference2007].

Coq [@thecoqdevelopmentteamCoqProofAssistant2022], being one of the most popular proof assistants, invested a lot effort into user-facing features: work on tactics like a new tactic engine [@spiwackVerifiedComputingHomological2011] and tactic languages (Ltac2 [@pedrotLtac2TacticalWarfare2019], SSReflect [@gonthierSmallScaleReflection2008], etc.), the introduction of a virtual machine for performance [@gregoireCompiledImplementationStrong2002] and others.
However, the implementation is quite hard to extend.
One either has to modify the source code, which is mostly limited to the core development team, as seen from the [contributors graph](https://github.com/coq/coq/graphs/contributors),
or one has to use Coq plugin system, which is rather challenging, and in the end, the complexity of it gave rise to TemplateCoq/MetaCoq [@malechaExtensibleProofEngineering2014  ; @sozeauMetaCoqProject2020].
While MetaCoq did open the possibility for some plugins [@nielsenFormalisingDecentralisedExchanges2023; @liesnikovGeneratingInductionPrinciples2020; @forsterCertifyingExtractionTime2019] to be written in a simpler way, their capabilities are still limited.

Agda has historically experimented a lot with different extensions to both the type system and the elaborator, even though the design does not accommodate these changes naturally.
Instead, each of these extensions is spread throughout many different parts of the code base[^agda-features-link].

Lean introduced elaborator extensions [@leonardodemouraLeanMetaprogramming2021; @ullrichNotationsHygienicMacro2020].
They allow the user to overload the commands, but if one defines a particular elaborator it becomes hard to interleave with others.
In a way, this is an imperative view on extensibility.

Idris [@bradyIdrisGeneralpurposeDependently2013; @christiansenElaboratorReflectionExtending2016] appeared as a programming language first and proof-assistant second and does not provide either a plugin or hook system at all, except for reflection.
Idris also focuses on tactics as the main mechanism for elaboration.

Turnstile+ by @changDependentTypeSystems2019 uses macros to elaborate surface syntax to a smaller core.
Macros allow them to modularly implement individual features, however combining different features requires the user to re-define all macros from scratch. This is the same problem as the one we mentioned for Lean.

TypOS [@allaisTypOSOperatingSystem2022a; @guillaumeallaisTypOS2022] is perhaps the closest to our work, but there are two important differences.
First, it is a domain-specific language for building type-checkers, while our design is language-agnostic, as long as the host language can model extensible datatypes in some capacity.
Second, their approach settles features of the language as they are decided by the main developer and does not concern future changes and evolution.
Finally, we try to stay close to the designs of existing dependently typed languages and offer flexibility in terms of choices, while TypOS requires the developer to start from scratch and restricts certain capabilities like overlapping rules for unification.

[^ghc-note]: [gitlab.haskell.org/ghc/ghc/-/wikis/plugins/type-checker/notes](https://gitlab.haskell.org/ghc/ghc/-/wikis/plugins/type-checker/notes)

[^agda-features-link]: some recent examples: [github.com/agda/agda/pull/6385](https://github.com/agda/agda/pull/6385),
[github.com/agda/agda/pull/6354](https://github.com/agda/agda/pull/6354).

# Future work #

We see three main prospects for future work:

* **Exploration of different kinds of metavariables.**
  Currently, we implement metavariables only for terms, while for some applications such as erasure [@tejiscakDependentlyTypedCalculus2020] or irrelevance inference, it would be beneficial to have metavariables representing erasure and relevance annotations.
  Additionally, we can introduce metavariables for names and implement data constructor disambiguation more simply, which would remove the current need to block on the expected type of overloaded constructors.
* **Rendering more elements of the elaborator as constraints.** Currently, components such as the occurs checking and reduction are simple function calls.
  Including them in the constraint machinery would make the implementation more uniform and allow users to extend them.
* **Error messages** are not the focus of present work, but it would be interesting to see if we can incorporate ideas by @heerenScriptingTypeInference2003 into our system.
* **Potential optimisations**.
  Currently, our system has a lot of room for potential optimisations.
  The first step would be to allow handlers to pass some information to their respective solvers, which is conceptually an easy change but technically requires introduction of an existential type in the plugin.
  Additionally, some expandable per-plugin store for the solvers would be useful, for example, to avoid recompututation of type class instances on every invocation.
  Somewhat more ambitiously, one can imagine a caching system for constraints, avoiding the need for solving the same constraint more than once.
  In particular, caching of reduction seems like it would be beneficial since we currently do a lot of redundant computations.
  However, the memory usage of such a caching system might be prohibitive.
  Finally, we would also like to explore possibilities for concurrent solving, similar to the plans of @allaisTypOSOperatingSystem2022a to use LVars for representing metavariables [@kuperLatticebasedDataStructures2015].

::: {#refs}
:::

<!---
Local Variables:
eval: (progn (olivetti-mode 't) (flyspell-mode 't) (flyspell-buffer) (flycheck-buffer) (ispell-change-dictionary "en_GB-w_accents"))
reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/paper/bib.bib") ;; add reftex support
End:
-->
