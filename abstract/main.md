---
title: 'Extensible elaborator design'

output: pdf_document

documentclass: easychair
classoption:
    - a4paper

# remove this before submission?
colorlinks: true

header-includes: |
    \author{Bohdan Liesnikov\inst{1}, Jesper Cockx\inst{1}}
    \authorrunning{Bohdan Liesnikov, Jesper Cockx}
    \titlerunning{Extensible elaborator design}
    \institute{TU Delft, Delft, Netherlands}
    \usepackage{todonotes}
    \definecolor{darkblue}{rgb}{0,0,0.5}
    \definecolor{darkgreen}{rgb}{0,0.3,0}
    \definecolor{darkpink}{rgb}{0.4,0,0.3}
    \definecolor{graygreen}{rgb}{0.3,0.5,0.3}
    \definecolor{grayblue}{rgb}{0.2,0.2,0.6}
    \definecolor{grayred}{rgb}{0.5,0.2,0.2}
    \lstset{ % stolen from build systems paper github.com/snowleopard/united/blob/main/paper/main.tex
      backgroundcolor=\color{white},     % choose the background color; you must add \usepackage{color} or \usepackage{xcolor}; should come as last argument
      %identifierstyle=\color{red},
      basicstyle=\small\ttfamily,   % the size of the fonts that are used for the code
      breakatwhitespace=false,           % sets if automatic breaks should only happen at whitespace
      breaklines=false,                  % sets automatic line breaking
      captionpos=b,                      % sets the caption-position to bottom
      abovecaptionskip=-3 mm,
      commentstyle=\itshape\color{graygreen}, % comment style
      % escapeinside={(:}{:)},           % if you want to add LaTeX within your code
      escapechar={!},
      % extendedchars=true,              % lets you use non-ASCII characters; for 8-bits encodings only, does not work with UTF-8
      % firstnumber=1000,                % start line enumeration with line 1000
      % frame=tb,                        % adds a frame around the code
      % keepspaces=true,                 % keeps spaces in text, useful for keeping indentation of code (possibly needs columns=flexible)
      keywordstyle=\color{darkblue},     % keyword style
      language=Haskell,                  % the language of the code
      morekeywords={}, % doesn't work for some reason
      deletekeywords={instance, data, where, class},      % if you want to delete keywords from the given language
      emph={data, class, instance, where, type},
      emphstyle=\color{darkpink},
      numbers=none,                      % where to put the line-numbers; possible values are (none, left, right)
      % numbersep=5pt,                   % how far the line-numbers are from the code
      % numberstyle=\tiny\color{mygray}, % the style that is used for the line-numbers
      % rulecolor=\color{black},         % if not set, the frame-color may be changed on line-breaks within not-black text (e.g. comments (green here))
      % showspaces=false,                % show spaces everywhere adding particular underscores; it overrides 'showstringspaces'
      % showstringspaces=false,          % underline spaces within strings only
      % showtabs=false,                  % show tabs within strings adding particular underscores
      % stepnumber=2,                    % the step between two line-numbers. If it's 1, each line will be numbered
      stringstyle=\color{grayred},     % string literal style
      % tabsize=2,                       % sets default tabsize to 2 spaces
      % title=\lstname                   % show the filename of files included with \lstinputlisting; also try caption instead of title
      % xleftmargin=10pt,
      aboveskip=4pt,
      belowskip=4pt
    }
---

Dependent types are a useful tool for statically enforcing properties of programs and for enabling type-driven development.
However, one reason why they have not yet been adopted more widely is because dependent type checkers are notoriously difficult to implement.
We present work-in-progress on a new design for elaboration of dependently-typed languages based on the idea of an open datatype for constraints.
This allows for a more compact base elaborator implementation while enabling extensions to the type system.
We don't require modifications to the core of type-checker, therefore preserving safety of the language.

**Introduction.**
The usual design of a compiler for a dependently-typed language consist of four main parts: a parser, an elaborator, a core type-checker, and a back-end.
Some languages omit some parts, such as Agda which lacks a full core type-checker.
The elaborator can be divided into two parts: traversal of the terms with collection of the constraints and solving of the constraints [@bruijnPleaWeakerFrameworks1991].
These can be found in all major dependently-typed languages like Idris, Coq, Lean, and Agda, though at times interleaved.
Agda perhaps pushes the idea of constraints the furthest of them all and internally has a family of [17 kinds of constraints](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Monad/Base.hs#L1064-L1092).
We will focus on it specifically below since the problems are most prominent there.

**Problems with unifiers.**
The most common constraint type is equality, which is typically solved by a unifier.
In order to provide the most powerful inference to users, compiler writers often extend the unifier to make it more powerful, which leads to complex and intricate code.
This code is also heavily used throughout the compiler (either as direct functions `leqType` when type-checking terms, `compareType` when type-checking applications, or as raised constraints `ValueCmp` and `SortCmp` from `equalTerm` while checking applications or definitions, `ValueCmpOnFace` from `equalTermOnFace` again while checking applications), making it sensitive towards changes and hard to maintain and debug.

An example from Agda's conversion checker is `compareAs` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L146-L218) which provides type-driven conversion checking and yet the vast majority of it is special cases for metavariables.
This function calls the `compareTerm'` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L255-L386) which then calls the `compareAtom` [function](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L419-L675).
Each of the above functions implements part of the "business logic" of the conversion checker with the total line count above 400 lines.
But each of them contains a lot of code dealing with bookkeeping related to metavariables and constraints: they have to throw and catch exceptions, driving the control flow of the unification, compute blocking tags that determine when a postponed constraint is retried,and deal with cases where either or both of the sides equation or its type are either metavariables or the reduction is blocked on one.
As a result this code is unintuitive and full of intricacies as indicated by [multiple](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L430-L431) [comments](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L521-L529).

Zooming in on the `compareAtom` function, the actual logic can be expressed in about [20 lines](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Conversion.hs#L530-L579) of simplified code.
This is precisely what we'd like the compiler developer to write, not to worry about the dance around the constraint system.

The functions described above are specific to Agda but in other major languages we can find similar problems with unifiers being large modules that are hard to understand.
The sizes of modules with unifiers are as follows: Idris ([1.5kloc](https://github.com/idris-lang/Idris2/blob/542ebeae97ed8b35ca1c987a56a61e98d4291a75/src/Core/Unify.idr#L1392-L1430)), Lean ([1.8kloc](https://github.com/leanprover/lean4/blob/75252d2b85df8cb9231020a556a70f6d736e7ee5/src/Lean/Meta/ExprDefEq.lean)), Coq ([1.8kloc](https://github.com/coq/coq/blob/155688103c43f578a8aef464bf0cb9a76acd269e/pretyping/evarconv.mli)).
For Haskell, which isn't a dependently-typed language yet but does have a constraints system [@jonesTypeInferenceConstraint2019], this number is at [2kloc](https://gitlab.haskell.org/ghc/ghc/-/blob/2f97c86151d7eed115ddcbdee1842684aed63176/compiler/GHC/Core/Unify.hs).

**How do we solve this?**
While Agda relies on constraints heavily, the design at large doesn't put at them the centre of the picture and instead frames as a gadget.
To give a concrete example, functions `noConstraints` or `dontAssignMetas` allows you to pose restrictions on a computation that rely on some behaviour of the constraint solver system throughout the rest of the codebase.
`abortIfBlocked`, `reduce` and `catchConstraint`/`patternViolation` force the programmer to make a choice between letting the constraint system handle blockers or doing it manually.
These things are known to be brittle and pose an increased mental overhead when writing a type-checker.

Our idea for a new design is to shift focus more towards the constraints themselves:
First we give a stable API for raising constraints so that instead of the type-checker carefully calling the right procedure we raise a constraint, essentially creating an "ask" to be fulfilled by the solvers. This isn't dissimilar to the idea of mapping object-language unification variables to host-language ones as done by @guidiImplementingTypeTheory2017, view of the "asks" as a general effect [@bauerEqualityCheckingGeneral2020, ch. 4.4] or communication with an independent process [@allaisTypOSOperatingSystem2022a].
Second, to make the language more modular we make constraints an extensible data type in the style of @swierstraDataTypesCarte2008 and give an API to define new solvers with the ability to specify what kinds of constraints they match on.
Our current prototype is implemented in Haskell as is available at [github.com/liesnikov/extensible-elaborators](https://github.com/liesnikov/extensible-elaborators).\todo{make the repo public}

For example, to solve unification problems we need to define a constraint that models them:
```haskell
data EqualityC e = EqualityCC Term Term Type
```

On the solver side we provide a suite of unification solvers that handle different cases of the problem.
Let's take a look at the simplest example -- syntactically equal terms.
``` haskell
syntacticH :: (MonadElab m, EqualityC :<: c)=> Constraint c -> m Bool
syntacticS :: (MonadElab m, EqualityC :<: c)=> Constraint c -> m ()
syntactic = Plugin {handler=syntacticH, solver=syntacticS,
                    pre=[...], suc=[...], tag="syntactic"}
```

We first define the class of constraints that will be handled by the solver by providing a "handler" -- a function that decides whether a given solver has to fire.
In this case -- checking that the constraint given is indeed an `EqualityC` and that the two terms given to it are syntactically equal.
The solver in this case simply marks the constraint as solved, since it only fires once it's been cleared to do so by a handler.
We separate the handler from the solver to allow for cheaper decision procedures and more expensive, effectful solvers.
Finally, we register the solver by declaring it using a plugin interface specifying solvers that precede and succeed it.
This plugin symbol will be picked up by the linker and registered at the runtime.

**Open constraint datatype.**
Refactoring the unifier into smaller solvers results in a compact elaborator for a simple language.
However, making the constraint datatype open and allowing users to register new solvers allows us to extend the language without affecting the core.
For example, to add implicit arguments to the language it's enough to extend the parser, add one case to the elaborator to add a new meta for every implicit and register a solver.
For a simple implicit every such metavariable will be instantiated by the unifier.

Once we have implicits as a case in the elaborator it should be possible to extend this system to accommodate for type classes [@hallTypeClassesHaskell1996], tactic arguments [@theagdateamAgdaUserManual2022, ch. 3.16.1] (assuming tactics) with just additional solvers and parsing rules.
We hope to also implement coercive subtyping (akin to [@aspertiCraftingProofAssistant2007]) and, perhaps, row types [@gasterPolymorphicTypeSystem1996].

\newpage

::: {#refs}
:::

<!---
Local Variables:
eval: (progn (olivetti-mode 't) (flyspell-mode 't) (flyspell-buffer)) ;; because it looks better this way!
reftex-default-bibliography: ("/home/bohdan/delft/extended-elab/extended-elab/abstract/bib.bib") ;; add reftex support
End:
-->
