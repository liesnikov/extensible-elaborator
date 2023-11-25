-- | things typechecker actions and constraints can be blocked on

module TypeCheck.Blockers ( Blocker(..)
                          , blockOnMeta
                          , unblockAMeta
                          , blockOnConstraint
                          , unblockAConstraint
                          , emptyBlocker
                          , andBlockers
                          , orBlockers
                          ) where

import           Data.Maybe
import           Data.Set as Set
import           Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as PP (text)
import           PrettyPrint

import TypeCheck.Constraints as C ( ConstraintId, ConstraintF, getConstraintId)
import Syntax.InternalSyntax ( MetaClosure(MetaVarClosure)
                             , MetaVarId
                             , Term(MetaVar) )

data Blocker = UnblockOnAll (Set Blocker)
             | UnblockOnAny (Set Blocker)
             | UnblockOnMeta MetaVarId     -- ^ Unblock if meta is instantiated
             | UnblockOnConstraint C.ConstraintId
  deriving (Show, Eq, Ord)

instance Disp Blocker where
  disp (UnblockOnAll bs) = PP.text "All {" <+> disp bs <+> PP.text "}"
  disp (UnblockOnAny bs) = PP.text "Any {" <+> disp bs <+> PP.text "}"
  disp (UnblockOnMeta mid) = PP.text "Meta: " <+> disp mid
  disp (UnblockOnConstraint cid) = PP.text "Constraint: " <+> disp cid


emptyBlocker :: Blocker
emptyBlocker = UnblockOnAny mempty

andBlockers :: Blocker -> Blocker -> Blocker
andBlockers (UnblockOnAll bs1) (UnblockOnAll bs2) = UnblockOnAll $ bs1 <> bs2
andBlockers (UnblockOnAll bs1) b2                = UnblockOnAll $ Set.insert b2 bs1
andBlockers b1                (UnblockOnAll bs2) = UnblockOnAll $ Set.insert b1 bs2
andBlockers b1                b2                 = UnblockOnAll $ Set.fromList [b1, b2]

orBlockers :: Blocker -> Blocker -> Blocker
orBlockers (UnblockOnAny bs1) (UnblockOnAny bs2) = UnblockOnAny $ bs1 <> bs2
orBlockers (UnblockOnAny bs1) b2                = UnblockOnAny $ Set.insert b2 bs1
orBlockers b1                (UnblockOnAny bs2) = UnblockOnAny $ Set.insert b1 bs2
orBlockers b1                b2                 = UnblockOnAny $ Set.fromList [b1, b2]

blockOnMeta :: Term -> Maybe Blocker
blockOnMeta (MetaVar (MetaVarClosure mid _)) = Just $ UnblockOnMeta mid
blockOnMeta _           = Nothing

blockOnConstraint :: C.ConstraintF f -> Blocker
blockOnConstraint = UnblockOnConstraint . C.getConstraintId

unblockAConstraint :: C.ConstraintId -> Blocker -> Maybe Blocker
unblockAConstraint cid (UnblockOnConstraint ucid) | cid == ucid = Nothing
                                                  | otherwise = Just $ UnblockOnConstraint ucid
unblockAConstraint cid (UnblockOnAll bs) =
  UnblockOnAll <$> unblockAll bs (unblockAConstraint cid)
unblockAConstraint cid (UnblockOnAny bs) =
  UnblockOnAny <$> unblockAny bs (unblockAConstraint cid)
unblockAConstraint cid (UnblockOnMeta mid) = Just $ UnblockOnMeta mid

unblockAMeta :: MetaVarId -> Blocker -> Maybe Blocker
unblockAMeta mid (UnblockOnMeta umid) | mid == umid = Nothing
                                      | otherwise = Just $ UnblockOnMeta umid
unblockAMeta mid (UnblockOnAll bs) =
  UnblockOnAll <$> unblockAll bs (unblockAMeta mid)
unblockAMeta mid (UnblockOnAny bs) =
  UnblockOnAny <$> unblockAny bs (unblockAMeta mid)
unblockAMeta mid (UnblockOnConstraint cid) = Just $ UnblockOnConstraint cid

unblockAny :: Set Blocker -> (Blocker -> Maybe Blocker) -> Maybe (Set Blocker)
unblockAny s ub = if Nothing `elem` unblocked then Nothing else Just reblocked
  where unblocked = Set.map ub s
        filtered = Set.filter (isJust) unblocked
        reblocked = Set.map (fromJust) filtered

unblockAll :: Set Blocker -> (Blocker -> Maybe Blocker) -> Maybe (Set Blocker)
unblockAll s ub = if Set.null reblocked then Nothing else Just reblocked
  where unblocked = Set.map ub s
        filtered = Set.filter (isJust) unblocked
        reblocked = Set.map (fromJust) filtered
