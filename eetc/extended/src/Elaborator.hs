module Elaborator where

import           Control.Monad.Except
    ( unless, MonadError(..), MonadIO(..), ExceptT, runExceptT )
import           Control.Monad.State (StateT(runStateT))
import           PrettyPrint ( SourcePos, render, D(..), Disp(..), Doc )
import           Text.PrettyPrint.HughesPJ ( ($$), nest, sep, text, vcat )
import qualified Unbound.Generics.LocallyNameless as Unbound

import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import           Environment (TcMonad)


-- | Marked locations in the source code
data SourceLocation where
  SourceLocation :: forall a. Disp a => SourcePos -> a -> SourceLocation

-- | An error that should be reported to the user
data Err = Err [SourceLocation] Doc

instance Disp Err where
  disp (Err [] msg) = msg
  disp (Err ((SourceLocation p term) : _) msg) =
    disp p
      $$ nest 2 msg
      $$ nest 2 (text "In the expression" $$ nest 2 (disp term))

-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations.
    ctx :: [I.Decl],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    globals :: Int,
    -- | Type declarations (signatures): it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    hints :: [I.Sig],
    -- | what part of the file we are in (for errors/warnings)
    sourceLocation :: [SourceLocation]
  }

-- | The initial environment.
emptyEnv :: Env
emptyEnv = Env {ctx = I.preludeDataDecls
               , globals = length I.preludeDataDecls
               , hints = []
               , sourceLocation = []
              }

instance Disp Env where
  disp e = vcat [disp decl | decl <- ctx e]

type ElabMonad = Unbound.FreshMT (StateT Env (ExceptT Err IO))

runElabMonad :: Env -> ElabMonad a -> IO (Either Err a)
runElabMonad env m =
  runExceptT $
    fmap fst $
    runStateT (Unbound.runFreshMT m) env

transEpsilon :: S.Epsilon -> I.Epsilon
transEpsilon S.Rel = I.Rel
transEpsilon S.Irr = I.Irr

transName :: S.TName -> I.TName
transName n =
  let s = Unbound.name2String n
      i = Unbound.name2Integer n
  in Unbound.makeName s i

transArg :: S.Arg -> ElabMonad I.Arg
transArg (S.Arg ep term) = I.Arg (transEpsilon ep) <$> (transTerm term)

transPattern :: S.Pattern -> ElabMonad I.Pattern
transPattern (S.PatCon dcname lpat) = do
  tlpat <- traverse
           (\(pat,ep) -> (,) <$> transPattern pat <*> (return $ transEpsilon ep))
           lpat
  return $ I.PatCon dcname tlpat
transPattern (S.PatVar tname) = return $ I.PatVar $ transName tname

transMatch :: S.Match -> ElabMonad I.Match
transMatch (S.Match match) = do
  (pat, body) <- Unbound.unbind match
  tpat <- transPattern pat
  tbody <-  transTerm body
  let tmatch = Unbound.bind tpat tbody
  return $ I.Match tmatch

transTerm :: S.Term -> ElabMonad I.Term
transTerm (S.Type) = return $ I.Type
transTerm (S.Var x) = return $ I.Var $ transName x
transTerm (S.Lam ep lam) = do
  (x, body) <- Unbound.unbind lam
  tbody <- transTerm body
  let tlam = Unbound.bind (transName x) tbody
  return $ I.Lam (transEpsilon ep) tlam
transTerm (S.App t arg) =
  I.App <$> (transTerm t) <*> (transArg arg)
transTerm (S.Pi ep typ pib) = do
  ttyp <- transTerm typ
  (x, body) <- Unbound.unbind pib
  tbody <- transTerm body
  let tpib = Unbound.bind (transName x) tbody
  return $ I.Pi (transEpsilon ep) ttyp tpib
transTerm (S.Ann term typ) =
  I.Ann <$> (transTerm term) <*> (transTerm typ)
transTerm (S.Pos spos term) = I.Pos spos <$> transTerm term
transTerm (S.TrustMe) = return I.TrustMe
transTerm (S.PrintMe) = return I.PrintMe
transTerm (S.Let term lbody) = do
  tterm <- transTerm term
  (x, body) <- Unbound.unbind lbody
  tbody <- transTerm body
  let tlbody = Unbound.bind (transName x) tbody
  return $ I.Let tterm tlbody
transTerm (S.TyUnit) = return I.TyUnit
transTerm (S.LitUnit) = return I.LitUnit
transTerm (S.TyBool) = return I.TyBool
transTerm (S.LitBool bool) = return $ I.LitBool bool
transTerm (S.If cond thent elset) =
  I.If <$> (transTerm cond) <*> (transTerm thent) <*> (transTerm elset)
transTerm (S.Sigma fe se) = do
  tfe <- transTerm fe
  (x, body) <- Unbound.unbind se
  tbody <- transTerm body
  let tse = Unbound.bind (transName x) tbody
  return $ I.Sigma tfe tse
transTerm (S.Prod fe se) = I.Prod <$> (transTerm fe) <*> (transTerm se)
transTerm (S.LetPair scrut lbody) = do
  tscrut <- transTerm scrut
  ((x, y), body) <- Unbound.unbind lbody
  tbody <- transTerm body
  let tlbody = Unbound.bind (transName x, transName y) tbody
  return $ I.LetPair tscrut tlbody
transTerm (S.TyEq fe se) = I.TyEq <$> (transTerm fe) <*> (transTerm se)
transTerm (S.Refl) = return $ I.Refl
transTerm (S.Subst body eq) = I.Subst <$> (transTerm body) <*> (transTerm eq)
transTerm (S.Contra c) = I.Contra <$> transTerm c
transTerm (S.TCon tcname largs) = I.TCon tcname <$> traverse transArg largs
transTerm (S.DCon dcname largs) = I.DCon dcname <$> traverse transArg largs
transTerm (S.Case scrut lmatch) = I.Case <$> (transTerm scrut) <*> (traverse transMatch lmatch)

elabTerm :: S.Term -> ElabMonad I.Term
elabTerm = undefined
