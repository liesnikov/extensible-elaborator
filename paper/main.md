---
title: 'Extensible elaborator design'
subtitle: Based on constraint-solving
author: Bohdan Liesnikov, Jesper Cockx

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

# Introduction #

Last decade brought a lot maturity into the dependently-typed language field.
Some of the bigger proof assistants like Coq [@teamCoqProofAssistant2022] invested a lot of effort into user-facing features. Some like Agda [@norellPracticalProgrammingLanguage] experimented with core features.
Lean [@mouraLeanTheoremProver2021] set out to become a default language for mathematics formalization, all the while bootstrapping the compiler. Idris [@bradyIdrisGeneralpurposeDependently2013; @christiansenElaboratorReflectionExtending2016] appeared as a programming language first and proof-assistant second. Andromeda [@bauerDesignImplementationAndromeda2018a; @bauerEqualityCheckingGeneral2020] appeared.

# Bidirectional typing #

\todo{explain how it's done usually}

# Constraint-based elaboration and design choices#

\todo{what do we do differently}

We intend to avoid problems described in [@henryModularizingGHC] by using a solution described in [@swierstraDataTypesCarte2008] and [@najdTreesThatGrow2017].

In particular, we intend to allow syntax extensions not only in the way of new constructors, but also in the annotations for the syntax.

# Case-studies #

## Implicit arguments ##

## Type classes ##

# Related work #

Coq [@teamCoqProofAssistant2022] being one of the most popular proof asssistants gained a lot of pace in development from investing effort into user-facing efforts: work on tactics like new tactic engine [?] and tactic languages (Ltac2 [@pedrotLtac2TacticalWarfare2019], SSReflect [@gonthierSmallScaleReflection2008], etc.), introduction of a virtual machine for performance [@gregoireCompiledImplementationStrong2002] and others.

Agda introduced a lot of experimental features, but isn't very modular [@HeavyCouplingHaskell], which hinders further change.

Lean introduced elaborator extensions [@leonardodemouraLeanMetaprogramming2021; @ullrichNotationsHygienicMacro2020a].
They allow the user to overload the commands, but if one defines a particular elaborator it becomes hard to interleave with others.
In a way, this is an imperative view on extensibility.


# Future work #

There are some things we leave for future work.
For example, coercive subtyping as implemented in Matita [@tassiBiDirectionalRefinementAlgorithm2012].
Or erasure inference [@tejiscakDependentlyTypedCalculus2020].

# References #

::: {#refs}
:::

<!---
Local Variables:
eval: (olivetti-mode) ;; because it looks better this way!
reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/paper/bib.bib") ;; add reftex support
End:
-->
