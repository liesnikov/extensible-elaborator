module Elaborator () where

import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import Environment (TcMonad)

translation :: S.Term -> I.Term
translation x = undefined

elaborateTerm :: S.Term -> TcMonad I.Term
elaborateTerm = undefined
