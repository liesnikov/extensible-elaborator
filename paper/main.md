---
title: 'Extensible elaborator design'
subtitle: draft
author: Bohdan Liesnikov, Jesper Cockx
date: \today

output: pdf_document

documentclass: scrartcl
geometry: "left=2cm,right=2cm,top=1cm,bottom=2cm"
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

# Abstract #



# Introduction #

Staticly-typed languages allow us to specify behavior of our programs more precisely.
This comes with a benefit of more static guarantees but with an increased toll on the user to supply more precise information.
Since the type of our program is part of the specification but we can use of the information in the type we can make use of the type to infer parts of our program.
This follows the idea of Connor McBride to "Write more types and fewer programs." [@ptoopTypeInferenceThought2022, @mcbrideEpigramPracticalProgramming2005 ch. 2.1]

The examples of these include overloaded functions in Java, implicits in Scala, type classes in Haskell.

In dependently-typed langauges our types can be much more precise.
This gives us an even bigger opportunity to infer larger parts of our programs from the type.
This includes higher-order unification for implicit arguments in Agda, implicit coercions in Coq, tactic arguments in Idris. \todo{something about Lean?}
The solving can be not only automatic but also interactive.
For example, holes in Agda, proof obligations and canonical structures [@mahboubiCanonicalStructuresWorking2013] in Coq, holes in Haskell [@koppelSearchingEntangledProgram2022]. \todo{look into program synthesis?}
All of these mechanisms use different solvers and have various degrees of extensiblity.
They are usually not isolated from each other and can therefore produce unexpected intractions (for example, in this case between implicits and instances [@PerformanceRegressionIssue]).

In all of these examples the solvers evolved organically over time together with the language.
Some like Coq [@teamCoqProofAssistant2022] invested a lot of effort into user-facing features while having a relatively stable core in the last decade but historically struggling with similar issues in the elaboration.
For example, Canonical Structures which didn't even get to be properly documented for 15 years.
Others like Agda [@norellPracticalProgrammingLanguage2007] experimented more with features baked into the core of the type system, like sized types which brought their own solver infrastructure [@abelExtensionMartinLofType2016].
Lean is a prominent example of a language that with bootstrapping [@mouraLeanTheoremProver2021] aims to bring more extensibility to the users [@leonardodemouraLeanMetaprogramming2021].

All of the languages above make use of the notion of metavariables (also known as "existential variables" [@teamCoqProofAssistant2022, ch. 2.2.1]) to represent an as of yet unknown part of the term.
Solving of metavariables is part of a process called elaboration, which turns user-friendly syntax into principled core syntax.
We propose a new architecture for an extensible elaborator for dependently-typed languages.
The idea is to provide an API that allows users to tap into the elaboration procedure with their own custom solvers that can manipulate metavariables and constraints placed on them.

Contributions:
* We propose a new design blueprint for an extensible language. It supports type classes, implicit arguments, implicit coercions, and tactic arguments.
* We provide a suite of solvers in lieu of common solvers like conversion checker in Agda.
* We suggest a new view on metavariables as communication channels for the solvers.
* We implement a prototype of a dependently-typed langauge with implicit arguments, type classes, etc. \todo{be honest about implementation}

We hope this goes towards understanding the art and science of implementing dependently-typed languages.
Making the implementations of different features of the language more independent.
This design separates the what the solvers are doing from the when.
Making it explicit what are the interaction points between them where the developer has to pay attention.
This might provide an inspiration for a library or a DSL for implementing dependently-typed languages.

# Constraint-based elaboration and design choices #

An example of that is Agda's typechecking of a [lambda function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Rules/Term.hs#L460-L578):


```haskell
checkLambda'
  :: Comparison          -- ^ @cmp@
  -> A.TypedBinding      -- ^ @TBind _ _ xps typ@
  -> List1 (NamedArg Binder)   -- ^ @xps@
  -> A.Expr              -- ^ @typ@
  -> A.Expr              -- ^ @body@
  -> Type                -- ^ @target@
  -> TCM Term
checkLambda' cmp b xps typ body target = do
  TelV tel btyp <- telViewUpTo numbinds target
  if size tel < numbinds || numbinds /= 1
    then (if possiblePath then trySeeingIfPath else dontUseTargetType)
    else useTargetType tel btyp
```

Jesper's idea of an example is for implicits - `implicitNamedArgs` [source](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/Implicit.hs#L99-L127).

Logic for the meta creation is in `newMetaArg`. [source](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/Implicit.hs#L147-L150).

Constraint solving logic `solveConstraint_`. [source](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/Constraints.hs#L244-L295).

Another idea from Jesper: conversion checker as an example of a big solver that has to be broken down. [source](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/Conversion.hs#L430). "`blockOnError` is code smell" says Jesper.
`compareAtom` part what you want to write is (almost) [this](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/Conversion.hs#L521-L594)


* Provide an example of a complex function to typecheck in Agda
* Break down features in terms of different plugins in our system
* Mention "Data-types a-la carte" [@swierstraDataTypesCarte2008]?
* Do we tackle anything mentioned in [@henryModularizingGHC]?
* There's a potential for [@najdTreesThatGrow2017], make a decision whether we're implementing it or not.



# Dependently-typed calculus and bidirectional typing #

In this section we describe the core for type system we implement as well as the core typing rules.
This work is based off pi-forall [@weirichImplementingDependentTypes2022] implementation.

We leave the core rules intact and therefore, the core calculus too.
This is dependently-typed calculus that includes Pi, Sigma and indexed inductive types.
Equality type isn't defined as a regular inductive type, but is instead built-in with the user getting access to the type and term constructor, but not able to pattern-matching on it, instead getting a `subst` primitive of type `(A x) -> (x=y) -> A y` and `contra` of type `forall A. True = False -> A`.


# Case-studies #

## Implicit arguments ##

## Type classes ##

# Limitations #

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


# Related work #

Coq [@teamCoqProofAssistant2022] being one of the most popular proof asssistants gained a lot of pace in development from investing effort into user-facing efforts: work on tactics like new tactic engine [@spiwackVerifiedComputingHomological2011] and tactic languages (Ltac2 [@pedrotLtac2TacticalWarfare2019], SSReflect [@gonthierSmallScaleReflection2008], etc.), introduction of a virtual machine for performance [@gregoireCompiledImplementationStrong2002] and others.

Agda introduced a lot of experimental features, but isn't very modular [@HeavyCouplingHaskell], which hinders further change.

Lean introduced elaborator extensions [@leonardodemouraLeanMetaprogramming2021; @ullrichNotationsHygienicMacro2020a].
They allow the user to overload the commands, but if one defines a particular elaborator it becomes hard to interleave with others.
In a way, this is an imperative view on extensibility.


\todo{this is a repetition of what was said previously}
Lean [@mouraLeanTheoremProver2021] set out to become a default language for mathematics formalization, all the while bootstrapping the compiler.
Idris [@bradyIdrisGeneralpurposeDependently2013; @christiansenElaboratorReflectionExtending2016] appeared as a programming language first and proof-assistant second. Andromeda [@bauerDesignImplementationAndromeda2018; @bauerEqualityCheckingGeneral2020] appeared as an experiment in providing a specification for

\todo{dump from introduction}
Dependently-typed language implementations usually consist of at least four parts:
parser, elaborator, core typechecker, and proper backend. The latter happens after typechecking and is not of a particular interest to us today.

We see parser or syntax extensibility as a necessary part of an extensible language.
However, this being an old problem means that there are existing solutions.
Macros are one of them and are utilized heavily in various forms in almost all established languages [@teamCoqProofAssistant2022; @theagdateamAgdaUserManual2022; @ullrichNotationsHygienicMacro2020a] and can be powerful enough to build a whole language around [@changDependentTypeSystems2019].

Core extensibility, on the other hand, appears to be a problem with too many degrees of freedom.
Andromeda [@bauerDesignImplementationAndromeda2018; @bauerEqualityCheckingGeneral2020] made an attempt at definitional equality, but is quite far from a usable dependently-typed language.
Agda's philosophy allows developers to experiment with the core, but also results in a larger amount of unexpected behaviors.
In general, modification of core rules will result in fundamental changes in the type theory, which can break plenty important properties like soundness or subject reduction.

This leaves us with the question of what can be achieved with the extensibility of an elaborator and why one would need it.
Elaborator is the part of the typechecker that performs all the desugaring to translate from the surface to the core language.
This includes type inference, implicit arguments inference, type classes, tactics, SMT integration.
Elaborators are often structured similarly to the core typechecker, i.e. following a bidirectional discipline of some sort. One can see that in Agda [@norellPracticalProgrammingLanguage2007], Matita [@tassiBiDirectionalRefinementAlgorithm2012], or in a paper by @ferreiraBidirectionalElaborationDependently2014.

# Future work #

There are some things we leave for future work.

* Coercive subtyping as implemented in Matita [@tassiBiDirectionalRefinementAlgorithm2012]
* Erasure inference [@tejiscakDependentlyTypedCalculus2020]
* Rendering of macros as constraints
* Mapping constraint solving onto a concurrent execution model. Use LVars here [@kuperLatticebasedDataStructures2015] here, similar to what TypOS [@allaisTypOSOperatingSystem2022a] is doing?

# References #

::: {#refs}
:::

<!---
Local Variables:
eval: (progn (olivetti-mode 't) (flyspell-mode 't) (flyspell-buffer)) ;; because it looks better this way!
reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/paper/bib.bib") ;; add reftex support
End:
-->
