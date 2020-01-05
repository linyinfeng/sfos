module Sfos.Evaluation
  ( eval
  , bigStep
  , smallStep
  )
where

import           Sfos.Syntax
import           Sfos.Support

eval :: Term -> Either Term Term
eval t = (if value stuckOrValue then Right else Left) stuckOrValue
  where stuckOrValue = bigStep t

bigStep :: Term -> Term
bigStep t = maybe t bigStep (smallStep t)

smallStep :: Term -> Maybe Term
smallStep (TmApp (TmAbs _ _ t12) v2) | value v2 = Just $ tmSubstTop v2 t12
smallStep (TmApp v1 t2) | value v1              = TmApp v1 <$> smallStep t2
smallStep (TmApp   t1                t2 )       = flip TmApp t2 <$> smallStep t1
smallStep (TmTyApp (TmTyAbs _ _ t12) ty2)       = Just $ tyTmSubstTop ty2 t12
smallStep (TmTyApp t1 ty2) = flip TmTyApp ty2 <$> smallStep t1
smallStep (TmLet _ v1 t2) | value v1            = Just $ tmSubstTop v1 t2
smallStep (TmLet x t1 t2) = flip (TmLet x) t2 <$> smallStep t1
smallStep (TmTyLet _ ty1 t2)                    = Just $ tyTmSubstTop ty1 t2
smallStep _                                     = Nothing
