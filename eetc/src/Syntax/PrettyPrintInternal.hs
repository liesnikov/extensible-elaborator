module Syntax.PrettyPrintInternal where

import Control.Monad.Reader (MonadReader (ask, local), asks)
import qualified Data.Set as S
import Text.PrettyPrint (($$), (<+>))
import qualified Text.PrettyPrint as PP
import qualified Unbound.Generics.LocallyNameless as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import PrettyPrint
import Syntax.InternalSyntax
import Syntax.ModuleStub

instance Disp (Unbound.Name Term) where
  disp = PP.text . Unbound.name2String

  -------------------------------------------------------------------------

-- * Disp Instances for Term syntax (defaults to Display, see below)

-------------------------------------------------------------------------

instance Disp Term

instance Disp Arg


instance Disp Pattern

instance Disp Match

------------------------------------------------------------------------

-- * Disp Instances for Modules

-------------------------------------------------------------------------

instance Disp [Decl] where
  disp = PP.vcat . map disp

instance Disp Epsilon where
  disp Irr = PP.text "irrelevant"
  disp Rel = PP.text "relevant"


instance Disp Module where
  disp m =
    PP.text "module" <+> disp (moduleName m) <+> PP.text "where"
      $$ PP.vcat (map disp (moduleImports m))
      $$ PP.vcat (map disp (moduleEntries m))

instance Disp ModuleImport where
  disp (ModuleImport i) = PP.text "import" <+> disp i

instance Disp Sig where
  disp (Sig n ep  ty) = brackets (isIrr ep) (disp n) <+> PP.text ":" <+> disp ty
    where
      isIrr :: Epsilon -> Bool
      isIrr Irr = True
      isIrr Rel = False

instance Disp Decl where
  disp (Def n term)  = disp n <+> PP.text "=" <+> disp term
  disp (RecDef n r)  = disp (Def n r)
  disp (TypeSig sig) = disp sig
  disp (Demote ep)   = PP.text "demote"

  disp (Data n params constructors) =
    PP.hang
      ( PP.text "data" <+> disp n <+> disp params
          <+> PP.colon
          <+> PP.text "Type"
          <+> PP.text "where"
      )
      2
      (PP.vcat $ map disp constructors)
  disp (DataSig t delta) =
    PP.text "data" <+> disp t <+> disp delta <+> PP.colon
      <+> PP.text "Type"


instance Disp ConstructorDef where
  disp (ConstructorDef _ c (Telescope [])) = PP.text c
  disp (ConstructorDef _ c tele) = PP.text c <+> PP.text "of" <+> disp tele


-------------------------------------------------------------------------

-- * Display instances for Prelude types used in AST

-------------------------------------------------------------------------

instance Display String where
  display = return . PP.text

instance Display Int where
  display = return . PP.text . show

instance Display Integer where
  display = return . PP.text . show

instance Display Double where
  display = return . PP.text . show

instance Display Float where
  display = return . PP.text . show

instance Display Char where
  display = return . PP.text . show

instance Display Bool where
  display = return . PP.text . show

-------------------------------------------------------------------------

-- * Display class instances for Terms

-------------------------------------------------------------------------


levelApp :: Int
levelApp     = 10
levelIf :: Int
levelIf      = 0
levelLet :: Int
levelLet     = 0
levelCase :: Int
levelCase    = 0
levelLam :: Int
levelLam     = 0
levelPi :: Int
levelPi      = 0
levelSigma :: Int
levelSigma   = 0
levelProd :: Int
levelProd    = 0
levelArrow :: Int
levelArrow   = 5

withPrec :: MonadReader DispInfo m => Int -> m a -> m a
withPrec p t =
  local (\d -> d { prec = p }) t

parens :: Bool -> Doc -> Doc
parens b = if b then PP.parens else id

brackets :: Bool -> Doc -> Doc
brackets b = if b then PP.brackets else id

instance Display (Unbound.Name Term) where
  display = return . disp

instance Display Term where
  display Type = return $ PP.text "Type"
  display (Var n) = display n
  display a@(Lam _ b) = do
    n <- ask prec
    (binds, body) <- withPrec levelLam $ gatherBinders a
    return $ parens (levelLam < n) $ PP.hang (PP.text "\\" PP.<> PP.sep binds PP.<> PP.text ".") 2 body
  display (App f x) = do
    n <- ask prec
    df <- withPrec levelApp (display f)
    dx <- withPrec (levelApp+1) (display x)
    return $ parens (levelApp < n) $ df <+> dx
  display (Pi ep a bnd) = do
    Unbound.lunbind bnd $ \(n, b) -> do
      p <- ask prec
      lhs <-
            if n `elem` toListOf Unbound.fv b
              then do
                dn <- display n
                da <- withPrec 0 (display a)
                return $ mandatoryBindParens ep  (dn <+> PP.colon <+> da)
              else do
                case ep of
                  Rel -> withPrec (levelArrow+1) (display a)
                  Irr -> PP.brackets <$> (withPrec 0 (display a))
      db <- withPrec levelPi (display b)
      return $ parens (levelArrow < p) $ lhs <+> PP.text "->" <+> db
  display (Ann a b) = do
    sa <- ask showAnnots
    if sa then do
      da <- withPrec 0 (display a)
      db <- withPrec 0 (display b)
      return $ PP.parens (da <+> PP.text ":" <+> db)
      else display a
  display (Pos _ e) = display e
  display TrustMe = do
    return $ PP.text "TRUSTME"
  display PrintMe = do
    return $ PP.text "PRINTME"
  display TyUnit = return $ PP.text "Unit"
  display LitUnit = return $ PP.text "()"
  display TyBool = return $ PP.text "Bool"
  display (LitBool b) = return $ if b then PP.text "True" else PP.text "False"
  display (If a b c) = do
    p <- ask prec
    da <- withPrec 0 $ display a
    db <- withPrec 0 $ display b
    dc <- withPrec 0 $ display c
    return $ parens (levelIf < p) $
      PP.text "if" <+> da <+> PP.text "then" <+> db
        <+> PP.text "else"
        <+> dc
  display (Sigma tyA bnd) =
    Unbound.lunbind bnd $ \(x, tyB) -> do
      if x `elem` toListOf Unbound.fv tyB then do
        dx <- display x
        dA <- withPrec 0 $ display tyA
        dB <- withPrec 0 $ display tyB
        return $
          PP.text "{" <+> dx <+> PP.text ":" <+> dA
            <+> PP.text "|"
            <+> dB
            <+> PP.text "}"
        else do
          p <- ask prec
          dA <- withPrec levelSigma $ display tyA
          dB <- withPrec levelSigma $ display tyB
          return $ parens (levelSigma < p) (dA PP.<+> PP.text "*" PP.<+> dB)
  display (Prod a b) = do
    p <- ask prec
    da <- withPrec levelProd $ display a
    db <- withPrec levelProd $ display b
    return $ parens (levelProd < p) (da PP.<> PP.text "," PP.<> db)
  display (LetPair a bnd) = do
    da <- display a
    Unbound.lunbind bnd $ \((x, y), body) -> do
      p <- ask prec
      dx <- withPrec 0 $ display x
      dy <- withPrec 0 $ display y
      dbody <- withPrec 0 $ display body
      return $
        parens (levelLet < p) $
        (PP.text "let"
          <+> (PP.text "("
          PP.<> dx
          PP.<> PP.text ","
          PP.<> dy
          PP.<> PP.text ")")
          <+> PP.text "="
          <+> da
          <+> PP.text "in")
        $$ dbody
  display (Let a bnd) = do
    Unbound.lunbind bnd $ \(x, b) -> do
      p <- ask prec
      da <- display a
      dx <- display x
      db <- display b
      return $
        parens (levelLet < p) $
        PP.sep
          [ PP.text "let" <+> dx
              <+> PP.text "="
              <+> da
              <+> PP.text "in",
            db
          ]

  display (Subst a b) = do
    p <- asks prec
    da <- withPrec 0 $ display a
    db <- withPrec 0 $ display b
    return $
      parens (levelPi < p) $
      PP.fsep
        [ PP.text "subst" <+> da,
          PP.text "by" <+> db
        ]
  display (TyEq a b) = do
    p <- ask prec
    da <- withPrec (levelApp+1) $ display a
    db <- withPrec (levelApp+1) $ display b
    return $ PP.parens $ (da <+> PP.text "=" <+> db)
  display Refl = do
    return $ PP.text "Refl"
  display (Contra ty) = do
    p <- ask prec
    dty <- display ty
    return $ parens (levelPi < p) $ PP.text "contra" <+> dty


  display (isNumeral -> Just i) = display i
  display (TCon n args) = do
    p <- ask prec
    dn <- display n
    dargs <- withPrec (levelApp+1) $ mapM display args
    return $
      parens (levelApp < p && length args > 0) (dn <+> PP.hsep dargs)
  display (DCon n args) = do
    p <- ask prec
    dn <- display n
    dargs <- withPrec (levelApp+1) $ mapM display args
    return $
      parens (levelApp < p && length args > 0) (dn <+> PP.hsep dargs)
  display (Case scrut alts) = do
    p <- asks prec
    dscrut <- withPrec 0 $ display scrut
    dalts <- withPrec 0 $ mapM display alts
    let top = PP.text "case" <+> dscrut <+> PP.text "of"
    return $
      parens (levelCase < p) $
        if null dalts then top <+> PP.text "{ }" else top $$ PP.nest 2 (PP.vcat dalts)
  display (MetaVar m) = do
    p <- asks prec
    let number = Unbound.name2Integer m
    dnumber <- display number
    return $ PP.text "?_" <> dnumber


instance Display Arg where
  display arg =
    case argEp arg of
      Irr -> PP.brackets <$> withPrec 0 (display (unArg arg))
      Rel -> display (unArg arg)


instance Display Match where
  display (Match bd) =
    Unbound.lunbind bd $ \(pat, ubd) -> do
      dpat <- display pat
      dubd <- display ubd
      return $ PP.hang (dpat <+> PP.text "->") 2 dubd

instance Display Pattern where
  display (PatCon c [])   = display c
  display (PatCon c args) = do
    dc <- display c
    dargs <- mapM wrap args
    return $ dc <+> PP.hsep dargs
      where
        wrap (a@(PatVar _),ep)    = bindParens ep <$> display a
        wrap (a@(PatCon _ []),ep) = bindParens ep <$> display a
        wrap (a@(PatCon _ _),ep)  = mandatoryBindParens ep <$> display a

  display (PatVar x) = display x

instance Disp Telescope where
  disp (Telescope t) = PP.sep $ map (PP.parens . disp) t

-- instance Display Telescope where
--   display (Telescope t) = do
--     -- needs a Display instance for Decl
--     -- feels like going against the design
--     dt <- mapM display t
--     return $ PP.sep $ map (PP.parens . disp) dt

instance Display a => Display (a, Epsilon) where
  display (t, ep) = bindParens ep <$> display t

instance Display ConstructorDef where
  display (ConstructorDef pos dc tele) = do
    dn <- display dc
    let dt = disp tele
    return $ dn PP.<+> PP.text "of" PP.<+> dt



-------------------------------------------------------------------------

-- * Helper functions for displaying terms

-------------------------------------------------------------------------

gatherBinders :: Term -> DispInfo -> ([Doc], Doc)
gatherBinders (Lam ep b) =
  Unbound.lunbind b $ \(n, body) -> do
    dn <- display n
    let db = bindParens ep  dn
    (rest, body') <- gatherBinders body
    return (db : rest, body')
gatherBinders body = do
  db <- display body
  return ([], db)

precBindParens :: Epsilon -> Bool -> Doc -> Doc
precBindParens Rel b d = parens b d
precBindParens Irr b d = PP.brackets d

-- | Add [] for irrelevant arguments, leave other arguments alone
bindParens :: Epsilon -> Doc -> Doc
bindParens Rel d = d
bindParens Irr d = PP.brackets d

-- | Always add () or [], shape determined by epsilon
mandatoryBindParens :: Epsilon -> Doc -> Doc
mandatoryBindParens Rel d = PP.parens d
mandatoryBindParens Irr d = PP.brackets d

-------------------------------------------------------------------------

-- * LFresh instance for DisplayInfo reader monad

-------------------------------------------------------------------------

instance Unbound.LFresh ((->) DispInfo) where
  lfresh nm = do
    let s = Unbound.name2String nm
    di <- ask
    return $
      head
        ( filter
            (\x -> Unbound.AnyName x `S.notMember` dispAvoid di)
            (map (Unbound.makeName s) [0 ..])
        )
  getAvoids = asks dispAvoid
  avoid names = local upd
    where
      upd di =
        di
          { dispAvoid =
              S.fromList names `S.union` dispAvoid di
          }
