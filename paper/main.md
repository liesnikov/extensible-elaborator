---
title: 'Extensible elaborator design'
subtitle: draft
author: Bohdan Liesnikov, Jesper Cockx
date: \today

link-citations: true

output: pdf_document

mainfont: 'Source Serif 4'
sansfont: 'Source Sans 3'
monofont: 'Source Code Pro'

documentclass: scrartcl
geometry: "left=2cm,right=2cm,top=1cm,bottom=2cm"
parindent: 1cm
fontsize: 12pt
header-includes: |
    \usepackage{todonotes}
---

# Abstract #



# Introduction #

Extensibility of a language has always been a lucrative target for compiler writers.
Macros, compiler plugins in different languages tap into this desire.
The same ideas made their way into functional languages, such as Haskell [@ghcdevelopmentteamGlasgowHaskellCompiler].

In dependently-typed land last decade brought a lot maturity for the language implementations.
Some of the bigger proof assistants like Coq [@teamCoqProofAssistant2022] invested a lot of effort into user-facing features while having a relatively stable core.
Some like Agda [@norellPracticalProgrammingLanguage] experimented more with features baked into the core of the type system.
Lean is a prominent example of a language that with bootstrapping [@mouraLeanTheoremProver2021] aims to bring more extensibility to the users [@leonardodemouraLeanMetaprogramming2021].
We set out for a similar cause, aiming to find a design blueprint for a dependently-typed language.

Dependently-typed language implementations usually consist of at least four parts:
parser, elaborator, core typechecker, and backend.
Macros are an answer for the parser extensibility, included in various forms in almost all established languages [@teamCoqProofAssistant2022; @theagdateamAgdaUserManual2022].
Core extensibility, on the other hand, appears to be a problem with too many degrees of freedom.
Andromeda [@bauerDesignImplementationAndromeda2018a; @bauerEqualityCheckingGeneral2020] made an attempt at definitional equality, but is quite far from a usable dependently-typed language.
Agda's philosophy allows developers to experiment with the core, but also results in a larger amount of unexpected behaviors.

# Dependently-typed calculus and bidirectional typing #

In this section we describe the core for type system we implement as well as the core typing rules.
This work is based off pi-forall [@weirichImplementingDependentTypes2022] implementation.

We leave the core rules intact and therefore, the core calculus too.
This is dependently-typed calculus that includes Pi, Sigma and inductive types.
Inductive types can be indexed.
Equality type isn't defined as a regular inductive type, but is instead built-in with the user getting access to the type and term constructor, but not able to pattern-matching on it, instead getting a `subst` primitive of type `(A x) -> (x=y) -> A y`.

# Constraint-based elaboration and design choices#

\todo{what do we do differently}

We intend to avoid problems described in [@henryModularizingGHC] by using a solution described in [@swierstraDataTypesCarte2008] and [@najdTreesThatGrow2017].



# Case-studies #

## Implicit arguments ##

## Type classes ##

# Related work #

Coq [@teamCoqProofAssistant2022] being one of the most popular proof asssistants gained a lot of pace in development from investing effort into user-facing efforts: work on tactics like new tactic engine [?] and tactic languages (Ltac2 [@pedrotLtac2TacticalWarfare2019], SSReflect [@gonthierSmallScaleReflection2008], etc.), introduction of a virtual machine for performance [@gregoireCompiledImplementationStrong2002] and others.

Agda introduced a lot of experimental features, but isn't very modular [@HeavyCouplingHaskell], which hinders further change.

Lean introduced elaborator extensions [@leonardodemouraLeanMetaprogramming2021; @ullrichNotationsHygienicMacro2020a].
They allow the user to overload the commands, but if one defines a particular elaborator it becomes hard to interleave with others.
In a way, this is an imperative view on extensibility.



\todo{fix this}
Lean [@mouraLeanTheoremProver2021] set out to become a default language for mathematics formalization, all the while bootstrapping the compiler.
Idris [@bradyIdrisGeneralpurposeDependently2013; @christiansenElaboratorReflectionExtending2016] appeared as a programming language first and proof-assistant second. Andromeda [@bauerDesignImplementationAndromeda2018a; @bauerEqualityCheckingGeneral2020] appeared as an experiment in providing a specification for

# Future work #



There are some things we leave for future work.
For example, coercive subtyping as implemented in Matita [@tassiBiDirectionalRefinementAlgorithm2012].
Or erasure inference [@tejiscakDependentlyTypedCalculus2020].

# References #

::: {#refs}
:::

<!---
Local Variables:
eval: (progn (olivetti-mode 't) (flyspell-mode 't)) ;; because it looks better this way!
reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/paper/bib.bib") ;; add reftex support
End:
-->
