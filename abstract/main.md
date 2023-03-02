---
title: 'Extensible elaborator design'

output: pdf_document

documentclass: easychair
classoption:
    - a4paper

# remove this before submission?
colorlinks: true

header-includes: |
    \author{Bohdan Liesnikov\inst{1} \and Jesper Cockx\inst{1}}
    \authorrunning{Liesnikov and Cockx}
    \titlerunning{Extensible elaborator design}
    \institute{TU Delft}
    \usepackage{todonotes}
---

\begin{abstract}

Dependent types are a useful tool for statically enforcing properties of programs and for enabling type-driven development.
However, one reason why they have not yet been adopted more widely is because dependent type checkers are notoriously difficult to implement.
We present work-in-progress on a new design for elaboration of dependently-typed languages based on the idea of an open datatype for constraints.
This allows for a more compact base elaborator implementation while enabling extensions to the type system.
We don't require modifications to the core of type-checker, therefore preserving safety of the language.

\end{abstract}


#### Introduction ####  {#section_introduction}

The usual design of a compiler for a dependently-typed language consist of four main parts: a parser, an elaborator, a core type-checker, and a back-end.
Some languages omit some parts, such as Agda which lacks a full core type-checker.

Both the elaborator and the core type-checker can be divided into two parts: traversal of the terms and collection (followed by solving) of the constraints [@bruijnPleaWeakerFrameworks1991].
These can be found in all major dependently-typed languages like Idris, Coq, Lean, and Agda.

_Idris_ has only [one kind of constraints](https://github.com/idris-lang/Idris2/blob/e673d05a67b82591131e35ccd50fc234fb9aed85/src/Core/UnifyState.idr) with the only two constructors being equality constraint for two terms and for two sequences of terms.
Both of these are [solved by the unifier](https://github.com/idris-lang/Idris2/blob/542ebeae97ed8b35ca1c987a56a61e98d4291a75/src/Core/Unify.idr#L1392-L1430) in the module `Core.Unify` which spans over 1.5 thousand lines.

_Lean_ \todo[size=tiny, fancyline]{example from Lean}
[the wrong thing ](https://github.com/leanprover/lean4/blob/0a031fc9bbb43c274bb400f121b13711e803f56c/src/Lean/Meta/Match/Basic.lean#L161) and
[basic metavariable definitions](https://github.com/leanprover/lean4/blob/30199745ad205dab58ff80bd8eb9b212ac1e765f/src/Lean/Meta/Basic.lean) and
[some unification tactic?](https://github.com/leanprover/lean4/blob/30199745ad205dab58ff80bd8eb9b212ac1e765f/src/Lean/Meta/Tactic/UnifyEq.lean) and
[this seems like definitional equality checker?](https://github.com/leanprover/lean4/blob/30199745ad205dab58ff80bd8eb9b212ac1e765f/src/Lean/Meta/Basic.lean#L1550-L1561) which is linked to [these partial functions](https://github.com/leanprover/lean4/blob/75252d2b85df8cb9231020a556a70f6d736e7ee5/src/Lean/Meta/ExprDefEq.lean)

_Coq_ \todo[size=tiny,fancyline]{example from Coq}
[solving evars??](https://github.com/coq/coq/blob/4804c2b3479a447d75473b7d6b57be01bcb45cdf/pretyping/evarsolve.mli) and
[evared term type](https://github.com/coq/coq/blob/110921a449fcb830ec2a1cd07e3acc32319feae6/engine/eConstr.mli) and
[term type](https://github.com/coq/coq/blob/c609f9b8549e7e9a946f3d783f71f7cdca35c8cc/kernel/constr.mli) and
[unification](https://github.com/coq/coq/blob/61ed5bf56871768ca020f119baa963b69ffe56f3/pretyping/unification.mli) and
[unification for type inference](https://github.com/coq/coq/blob/155688103c43f578a8aef464bf0cb9a76acd269e/pretyping/evarconv.mli) and

_Agda_ perhaps pushes the idea of constraints the furthest of them all and internally has a family of [17 kinds of constraints](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Monad/Base.hs#L1064-L1092) that grew organically.
We will focus on Agda specifically below since there the problems are most prominent.

% Jesper: Perhaps also mention constraints in Haskell?

#### Problems with unifiers ####  {#section_unifier_problems}

As hopefully evident the most common constraint type is equality.
And the solver for it is typically called a unifier.
For a modern language it is expected that to implement higher-order unification which is notoriously hard since it is undecidable in general.

The complexity stems from the desire of compiler writers to implement the most powerful unifier, thus providing the most powerful inference to users.
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

#### How do we solve this ####  {#section_solution}

While Agda relies on constraints, the design at large doesn't put at the centre of the picture and instead is primarily seen as a gadget.
To give a concrete example, Agda's constraint [solver](https://github.com/agda/agda/blob/v2.6.2.2/src/full/Agda/TypeChecking/Constraints.hs#L251-L301) relies on the type-checker to call it at the point where it is needed and has to be carefully engineered to work with the rest of the code.

Our idea for a new design is to shift focus more towards the constraints themselves:

1. Give a stable API for raising constraints so that instead of the type-checker carefully calling the right procedure we raise a constraint, essentially creating an "ask" to be fulfilled by the solvers.\todo[size=tiny,fancyline]{This relates to TypOS [@allaisTypOSOperatingSystem2022a] and [@guidiImplementingTypeTheory2017] reference them here?}

2. Make constraints an extensible data type in the style of "Data types à la carte" [@swierstraDataTypesCarte2008] and give an API to define new solvers with the ability to specify what they match on.

\todo[size=tiny, fancyline, author=Jesper]{is another goal here also to be able to add new syntax to the language without having to mess around too much with metavariables? Or is that a separate concern?}

For example, to solve unification problems we need to define a constraint that denotes them:
```haskell
data EqualityConstraint e = EqualityConstraint Syntax.Term Syntax.Term Syntax.Type
```

On the solver side we provide a suite of unification solvers that handle different cases of the problem. \todo[size=tiny,fancyline,author=Jesper]{it would be nice if we could include a version of your picture showing how the relation is between the syntax traversal and the solvers.}
Let's take a look at the simplest example -- syntactically equal terms.
``` haskell
-- solves syntactically equal terms
syntacticSolverHandler :: (EqualityConstraint :<: c)
                       => Constraint c -> MonadElab Bool
syntacticSolver :: (EqualityConstraint :<: c)
                => Constraint c -> MonadElab Bool
syntactic :: Plugin
syntactic  = Plugin { solver  = syntacticSolver
                    , handler = syntacticSolverHandler
                    ...
                    }
```

We first define the class of constraints that will be handled by the solver via providing a "handler" -- function that decides whether a given solver has to fire.
\todo[size=tiny,fancyline,author=Jesper]{I'm thinking now of whether there is some connection with handlers from effect systems, could we see constraints as effects and solvers as handlers for them?}
In this case - checking that the constraint given is indeed an `EqualityConstraint` and that the two terms given to it are syntactically equal.
Then we define the solver itself,
which in this case doesn't have to do anything except mark the constraint as solved, since we assume it only fires once it's been cleared to do so by the handler.
The reason for this separation between a decision procedure and execution of it is to ensure separation between effectful and costly solving and cheap decision-making that should require only read-access to the state.

Finally, we register the solver by declaring it using a plugin interface.
This plugin symbol will be picked up by the linker and registered at the runtime.

Similarly, we can define solvers that only work on problems where one of the sides is a metavariable:

``` haskell
-- solve cases when one side is a metavariable
unifySolverL :: (EqualityConstraint :<: c)
             => Constraint c -> MonadElab Bool
unifySolverLHandler :: (EqualityConstraint :<: c)
                    => Constraint c -> MonadElab Bool

```
% Jesper: this is probably too much detail. Just showing one of the four type signatures above is probably enough.

Here the job of the solver is not as trivial -- it has to check that the type of the other side indeed matches the needed one and then register the instantiation of the metavariable in the state.
If both of those steps are successful we can return `True` and the constraint will be marked as solved.

In the cases above we don't have to worry about the order since the problems they match on don't overlap.
In the case they don't we can provide priority preferences:

``` haskell
complexSolver1 :: Constraint c -> MonadElab Bool
complexHandler1 :: Constraint c -> MonadElab Bool
complexSymbol1 = "complexSolver1"
complex1 = Plugin { ...
                  , symbol   = complexSymbol1
                  , precedes = [unifySolverLS, unifySolverRS]
                  , succeeds = []
                  }

complexSolver2 :: Constraint c -> MonadElab Bool
complexHandler2 :: Constraint c -> MonadElab Bool
complexSymbol2 = "complexSolver2"
complex2 = Plugin { ...
                  , symbol   = complexSymbol2
                  , precedes = [complexSymbol1]
                  , succeeds = []
                  }
```
% Jesper: I would probably hide this code completely and instead just describe how the priorities work
% in words, e.g. that you have a partial ordering on the names of the handlers.

At the time of running the compiler, these preferences are loaded into a big pre-order relation for all the plugins, which is then linearised and used to guide the solving procedure.

Solvers here have read access to the state which might e.g. verify that there are no additional constraints on the meta or verify that there's another one.

#### Extension of the system to include open constraint datatype ####

The system above should result in a compact base of an elaborator.
However, if now extend the constraint datatype to be open and allow users to register new solvers it allows us for a few extensions of this bare-bones type theory

##### Implicits #####

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

where `FillInTheTerm` is defined as follows:

```haskell
-- this terms has to be filled in
data FillInTheTerm e =
     FillInTheTerm Syntax.Term Syntax.Type
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

##### Tactic arguments #####

Similarly, we can declare a type `TacticArgument t A` which computes to `A`.
We need the parser to desugar a definition borrowed from Agda manual

```agda
clever-search : Term -> TC Term
clever-search hole = unify hole (lit (nat 17))

the-best-number : {@(tactic clever-search) n : Nat} -> Nat
the-best-number {n} = n
```

to the following one

```agda
the-best-number : Implicit (TacticArgument clever-search Nat) -> Nat
```

and whenever such a function is used to apply to an additional implicit argument

```agda
check : (the-best-number ?) = 17
check = refl
```

This should result in two constraints being raised:
```
C1 : FillInTheTerm ?_1 (Implicit (TacticArgument clever-search) Nat)
C2 : EqualityConstraint (the-best-number ?_1) 17
```

The first constraint is matched on by a tactic-runner constraint solver:

```haskell
tacticRunnerSolver :: (MonadElab m, FillInTheTerm :<: c)
                   -> Constraint c -> m ()
tacticRunnerSolver c = do
    (FillInTheTerm m T) <- match @FillInTheTerm c
    tactic <- createMeta
    typ <- createMeta
    raiseEqualityConstraint T (App (TCon "Implicit")
                                   [(App (TCon "TacticArgument")
                                         [tactic typ])] $ do
        res <- runTactic tactic typ
        raiseEqualityConstraint m res
```

##### Coercive subtyping #####

Similarly, we should be able to render coercions by inserting a `coerce : Implicit (Coercion A B) -> A -> B` function pessimistically by the parser.
Such `coerce` would compute to identity when the coercion is identity.

#### Future work ####

There are some things we leave for future work.

* Implement erasure inference [@tejiscakDependentlyTypedCalculus2020]?
* Implement Canonical structures [@mahboubiCanonicalStructuresWorking2013]?
* Row types?
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
