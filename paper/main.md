---
title: 'Extensible elaborator design'
subtitle: draft
author: Bohdan Liesnikov, Jesper Cockx
date: \today

output: pdf_document

documentclass: scrartcl
geometry: "left=2.5cm,right=2.5cm,top=1cm,bottom=2.2cm"
numbersections: true
colorlinks: true
link-citations: true
parindent: 1cm

fontsize: 12pt
mainfont: 'Source Serif 4'
seriffont: 'Source Serif 4'
sansfont: 'Source Sans 3'
monofont: 'Source Code Pro'

header-includes: |
    \usepackage{todonotes}
---

\begin{abstract}

We present a new design for compilers for dependently-typed languages based on the idea of open datatype for constraints.
This allows for more compact base elaborator implementation while enabling extensions to the type system.
We don't require modifications to the core of type-checker, therefore preserving safety of the language.

\end{abstract}


# Introduction #  {#section_introduction}

Staticly-typed languages allow us to specify behavior of our programs more precisely.
This comes with a benefit of more static guarantees but with an increased toll on the user to supply more precise information.
Since the type of our program is part of the specification but we can use of the information in the type we can make use of the type to infer parts of our program.
This follows the idea of Connor McBride to "Write more types and fewer programs." [@ptoopTypeInferenceThought2022; @mcbrideEpigramPracticalProgramming2005 chap. 2.1]

The examples of these include overloaded functions in Java, implicits in Scala, type classes in Haskell.

In dependently-typed languages our types can be much more precise.
This gives us an even bigger opportunity to infer larger parts of our programs from the type.
This includes higher-order unification for implicit arguments in Agda, implicit coercions in Coq, tactic arguments in Idris. \todo{something about Lean?}
The solving can be not only automatic but also interactive.
For example, holes in Agda, proof obligations and canonical structures [@mahboubiCanonicalStructuresWorking2013] in Coq, holes in Haskell [@koppelSearchingEntangledProgram2022]. \todo{look into program synthesis?}
All of these mechanisms use different solvers and have various degrees of extensiblity.
They are usually not isolated from each other and can therefore produce unexpected interactions (for example, in this case between implicits and instances [@PerformanceRegressionIssue]).

In all of these examples the solvers evolved organically over time together with the language.
Some like Coq [@teamCoqProofAssistant2022] invested a lot of effort into user-facing features while having a relatively stable core in the last decade but historically struggling with similar issues in the elaboration.
For example, Canonical Structures which didn't even get to be properly documented for 15 years.
Others like Agda [@norellPracticalProgrammingLanguage2007] experimented more with features baked into the core of the type system, like sized types which brought their own solver infrastructure [@abelExtensionMartinLofType2016].
Lean is a prominent example of a language that with bootstrapping [@mouraLeanTheoremProver2021] aims to bring more extensibility to the users [@leonardodemouraLeanMetaprogramming2021].

All of the languages above make use of the notion of metavariables (also known as "existential variables" [@teamCoqProofAssistant2022 chap. 2.2.1]) to represent an as of yet unknown part of the term.
Solving of metavariables is part of a process called elaboration, which turns user-friendly syntax into principled core syntax.
We propose a new architecture for an extensible elaborator for dependently-typed languages.
The idea is to provide an API that allows users to tap into the elaboration procedure with their own custom solvers that can manipulate metavariables and constraints placed on them.

Contributions:

* We propose a new design blueprint for an extensible language. It supports type classes, implicit arguments, implicit coercions, and tactic arguments.
* We provide a suite of solvers in lieu of common solvers like conversion checker in Agda.
* We suggest a new view on metavariables as communication channels for the solvers.
* We implement a prototype of a dependently-typed language with implicit arguments, type classes, etc.\todo{be honest about implementation}

We hope this goes towards understanding the art and science of implementing dependently-typed languages.
Making the implementations of different features of the language more independent.
This design separates the what the solvers are doing from the when.
Making it explicit what are the interaction points between them where the developer has to pay attention.
This might provide an inspiration for a library or a DSL for implementing dependently-typed languages.

# Constraint-based elaboration and design choices # {#section_constraint_elaboration}

In this section we present some typical design challenges that come up while building a dependently typed compiler, the way they are usually solved and what does the design blueprint we're suggesting bring to the picture.

## Current design space ##

Constraints have been an integral part of compiler for strongly-typed languages for a while.
For example, both Haskell [@vytiniotisOutsideInModularType2011] and Agda [@norellPracticalProgrammingLanguage2007 chap. 3] use constraints extensively.
In the former case they are even reflected and can be manipulated by the user [@orchardHaskellTypeConstraints2010a; @ghcdevelopmentteamGHCUserGuide chap. 6.10.3].
This has proved to be a profitable design decision for GHC, as is indicated, for example in the following talk by @jonesTypeInferenceConstraint2019 as well as in a few published sources [@vytiniotisOutsideInModularType2011; @jonesPracticalTypeInference2007].

However, in the land of dependently-typed languages constraints are much less principled.
Agda has [a family of constraints](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Monad/Base.hs#L1064-L1092) that grew organically, currently that's 17 constructors.
Idris technically [has constraints](https://github.com/idris-lang/Idris2/blob/e673d05a67b82591131e35ccd50fc234fb9aed85/src/Core/UnifyState.idr) with the only two constructors are equality constraints of sequence of terms. We discuss this further in [Related Work section](#section_related_work)

## Type-checking function application in the presence of implicit arguments ##

During function application typechecking there may be different kinds of arguments to process, for example instance arguments, implicit arguments, or tactic arguments.
If we start from a simple case of typechecking an application of a function symbol to regular arguments, every next extension requires to be handled in a special case.

Take Agda as an example: when checking an application during the [insertion of implicit arguments](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Implicit.hs#L99-L127) we already have to carry the information on how the argument will be resolved and then create a [new kind of meta variable](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Implicit.hs#L131-L150) for each of those cases.

Instead of handling every kind of metavarible in a distinct way we uniformly dispatch a search for the solution, which is then handled by the constraint solvers (in contrast with Idris [@bradyIdrisGeneralpurposeDependently2013 chap. ?], see more in the [Related work section](#section_related_work)).
We achieve this by creating metavariables for the unknown terms and then raising a constraint for the meta containing the type of the meta.
This constraint can be latched on by the right solver based on this type. 

In this view the elaborator for the application of a function doesn't have to know anything about the implicits at all.
The only thing we require is that the elaboration of the argument is called with the type infromation available.
This corresponds to how in bidrectional typing function application is done in the inference mode but the arguments are processed in checking mode.

``` haskell
inferType (App t1 t2) = do
  (et1, Pi tyA tyB) <- inferType t1
  et2 <- checkType t2 tyA
  return (App et1 et2, subst tyB et2)
```

```haskell
checkType (Implicit) ty = do
  m <- createMeta
  raiseConstraint $ FillInTheImplicit m ty
  return m
```

This metavariable in its own turn gets instantiated by a fitting solver.
The solvers match on the shape of the type that metavariable stands for and handle it in a case-specific manner: instance-search for type classes, tactic execution for a tactic argument.
If it is a regular implicit, however, the only solver that's needed is a trivial one that checks that the metavariable has been instantiated indeed.
This is because a regualr implicit should be instantiated by a unification problem encountered at some point later.
This servers as a guarantree that all implicits have been filled in.

Let us go through an example of the elaboration process for a simple term:

```
plus : {A : Type} -> {{PlusOperation A}} -> (a : A) -> (b : A) -> A

instance PlusNat : PlusOperation Nat where
  plus = plusNat

two = plus 1 1
```

We will step through elaboration of the term `two`.

1. First the pre-processor eliminates the implicits and typeclass arguments.
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
   C1: FillInTheImplicit ?_1 (Implicit Type)
   C2: FillInTheImplicit ?_2 (TypeClass PlusOperation (deImp ?_1))`
   C3: EqualityConstraint ?_1 Nat Type`
   C4: EqualityConstraint ?_1 Nat Type`
   ```
   
   The first two correspond to implicit arguments.
   The latter two are unification problems rendered into constraints.

3. Now we step into the constraint-solving world.
   First the unifier solves the latter two, instantitating `?_1` to `Nat`.
   Next the typeclass resolution launches a search for the instance, resolving `?_2` to the `PlusNat` instance.
   Finally, C1 is discarded as solved since `?_1` is already instantiated to `Nat`.


## Conversion checking in the presence of a meta-variables ##

Higher-order unification is notoriously hard to implement because it is undecidable in general.
The complexity stems from the desire of compiler writers to implemenmt the most powerful unifier.
This code is also heavily used throughout the compiler, making it sensitive towards changes and hard to maintain and debug. \todo{footnote about Agda CI on cubical and stdlib, Coq on unimath}
Some of this complexity is unavoidable, but we can manage it better by splitting it up into small modular components.
In practice this means thast one doesn't have to fit together an always-growing one conversion checker but can instead write different cases separately.
We again rely on the constraint solver machinery to distribute the problems to the fitting solvers.

An example from Agda's conversion checker is `compareAs` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L146-L218) that provides type-driven conversion checking.
The function is almost 90 lines long, and yet the vast majority of it are special cases of metavariables.
This function calls the `compareTerm'` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L255-L386), which itself is 130 lines.
`compareTerm'` calls the `compareAtom` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L419-L675).
Which itself is almost 200 lines of code.
Each of the above functions implements part of the "business logic" of the conversion checker.
But each of them containts a lot of code dealing for bookkeeping realted to metavariables and constraints:
1. They has to throw and catch exceptions, driving the control flow of the unification.
2. They has to compute blocking tags that determine when a postponed constraint is retried.
3. They has to deal with cases where either or both of the sides of the equation or its type are either metavariables or terms whose evaluation is blocked on some metavariable.

This code is unintuitive and full on intricacies as indicated by [multiple](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L430-L431) [comments](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L521-L529).

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

\todo{look at Idris two too}

This is precisely what we'd like the compiler developer to write, not to worry about the dance around the constraint system.

## How is this achieved ##

Traditionally the constraint-solving is viewed as a gadget to postpone problems that can't be solved at the moment, and not the center piece of the elaboration procedure.
This can be primarily observed in the design of the [solver](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Constraints.hs#L251-L301), where the code around it relies on the typechecker to call it at the point where it is needed.

Our core idea to build the new design is to put the constraint solving at the very heart of the elaborator.

* Mention "Data-types a-la carte" [@swierstraDataTypesCarte2008]?
* Do we tackle anything mentioned in [@henryModularizingGHC]?
* There's a potential for [@najdTreesThatGrow2017], make a decision whether we're implementing it or not.

# Dependently-typed calculus and bidirectional typing # {#section_bidirectional}

In this section we describe the core for type system we implement as well as the core typing rules.
This work is based off pi-forall [@weirichImplementingDependentTypes2022] implementation.

We leave the core rules intact and therefore, the core calculus too.
This is dependently-typed calculus that includes Pi, Sigma and indexed inductive types.
Equality type isn't defined as a regular inductive type, but is instead built-in with the user getting access to the type and term constructor, but not able to pattern-matching on it, instead getting a `subst` primitive of type `(A x) -> (x=y) -> A y` and `contra` of type `forall A. True = False -> A`.


# Case-studies # {#section_casestudies}

## Implicit arguments ##

## Type classes ##

# Limitations # {#section_limitations}

## Handling of meta-variables outside of definition sites ##

After we elaborate a definition there can still be unsolved metas in it.
This presents us with a design choice.
First option is to freeze the metas and instantiate them per-use site, essentially allowing for an expression to have multiple types.
Second option is to leave them up to be solved later, which might make elaboration less predictable, since now the use sites can influence whether a particular definition type-checks.
Third option is to report them as an error and exit immediately.

In particular, second option allows us to incorporate more involved inference algorithms into the system.
For example, if we were to implement an erasure inference algorithm as described by
Tejiščák [@tejiscakDependentlyTypedCalculus2020], we would have to create metavariable annotations (described as "evars" in the paper) that can be instantiated beyond definition site.


## The language is only as extensible, as core is ##

Extensibility via constraints allows for a flexible user-specified control flow as soon as we step into the constraints world.
But control flow of the main body of the basic language elaborator is fixed by the basic language developer.
For example, consider the following simplified [lambda-function typechecking function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Rules/Term.hs#L460-L578) from Agda: \todo{refer to the previous usage of such a function}

``` haskell
checkLambda' cmp b xps typ body target = do
  TelV tel btyp <- telViewUpTo numbinds target
  if size tel < numbinds || numbinds /= 1
    then dontUseTargetType
    else useTargetType tel btyp
```

Here Agda steps away from the bidirectional discipline and infers a (lambda) function if the target type isn't fully known.
If in our design the developer chooses to go only with pure bi-directional style of type-checking inferred lambda-functions would be impossible to emulate.
That is, unless one essentially renders macros and writes their own typechecking case for an inferrable lambda.

In order to gain this extra bit of flexibility we provide `inferType` case for lambdas, even though our base language doesn't use it. \todo{actually write this case}

## Reliance of the pre-processor ##

This work crucially relies on a pre-processor of some kind, be it macro expansion or some other way to extend the parser with custom desugaring rules.
In particular, in order to implement n-ary implicit arguments correctly and easily we need the pre-processor to expand them to the right arity.
For coercions we need to substitute every term `t` in the coercible position for `coerce _ t`.
This can impact performance.

Alternatively, one can imagine a system where constraint solvers are latching onto non-reduced types and terms in constraints.
In that case we can get around with a trick borrowed from Coq, where `coerce _ t` computes to `_ t`, but since we typechecked an unreduced application the search will still be launched on the right form.

# Related work # {#section_related_work}

We are certainly not the first ones to try to tackle extensibility of a language implementation.
This section is structured according to the part of the compiler pipeline that allows for the extensibility.
Dependently-typed language implementations usually consist of at least four parts:
parser, elaborator, core typechecker, and proper backend.
The back-end part is currently irrelevant to our interests, since for a language to be specified usually means for specification of the core, anything that happens beyond after core doesn't extend the language, but rather tries to preserve its semantics in some form.
Therefore we're left with three parts: parser, elaborator and core typechecker.

We see parser or syntax extensibility as a necessary part of an extensible language.
This problem has been studied extensively in the past and has a multitude of existing solutions.
Macros are one of them and are utilized heavily in various forms in almost all established languages [@teamCoqProofAssistant2022; @theagdateamAgdaUserManual2022; @ullrichNotationsHygienicMacro2020a] and can be powerful enough to build a whole language around [@changDependentTypeSystems2019].

Core extensibility, on the other hand, appears to be a problem with too many degrees of freedom.
Andromeda [@bauerDesignImplementationAndromeda2018; @bauerEqualityCheckingGeneral2020] made an attempt at definitional equality, but is quite far from a usable dependently-typed language.
Agda's philosophy allows developers to experiment with the core, but also results in a larger amount of unexpected behaviors.
In general, modification of core rules will result in fundamental changes in the type theory, which can break plenty important properties like soundness or subject reduction.

This leaves us with the question of what can be achieved with the extensibility of an elaborator and why one would need it.
Elaborator is the part of the typechecker that performs said desugaring to translate from the surface to the core language.
This includes type inference, implicit arguments inference, type classes, tactics, SMT integration.
Elaborators are often structured similarly to the core typechecker, i.e. following a bidirectional discipline of some sort. One can see that in Agda [@norellPracticalProgrammingLanguage2007], Matita [@tassiBiDirectionalRefinementAlgorithm2012], or in a paper by @ferreiraBidirectionalElaborationDependently2014.

Coq [@teamCoqProofAssistant2022] being one of the most popular proof asssistants gained a lot of pace in development from investing effort into user-facing efforts: work on tactics like new tactic engine [@spiwackVerifiedComputingHomological2011] and tactic languages (Ltac2 [@pedrotLtac2TacticalWarfare2019], SSReflect [@gonthierSmallScaleReflection2008], etc.), introduction of a virtual machine for performance [@gregoireCompiledImplementationStrong2002] and others.

Agda introduced a lot of experimental features, but isn't very modular [@HeavyCouplingHaskell], which hinders further change.

Lean introduced elaborator extensions [@leonardodemouraLeanMetaprogramming2021; @ullrichNotationsHygienicMacro2020a].
They allow the user to overload the commands, but if one defines a particular elaborator it becomes hard to interleave with others.
In a way, this is an imperative view on extensibility.

Idris [@bradyIdrisGeneralpurposeDependently2013; @christiansenElaboratorReflectionExtending2016] appeared as a programming language first and proof-assistant second.


# Future work #

There are some things we leave for future work.

* Coercive subtyping as implemented in Matita [@tassiBiDirectionalRefinementAlgorithm2012]
* Erasure inference [@tejiscakDependentlyTypedCalculus2020]
* Rendering of macros as constraints
* Mapping constraint solving onto a concurrent execution model. Use LVars here [@kuperLatticebasedDataStructures2015] here, similar to what TypOS [@allaisTypOSOperatingSystem2022a] is doing?

::: {#refs}
:::

<!---
Local Variables:
eval: (progn (olivetti-mode 't) (flyspell-mode 't) (flyspell-buffer)) ;; because it looks better this way!
reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/paper/bib.bib") ;; add reftex support
End:
-->
