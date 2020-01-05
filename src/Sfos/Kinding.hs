module Sfos.Kinding
  ( kindOf
  )
where

import           Sfos.Syntax
import           Sfos.Support
import           Sfos.Error
import           Control.Exception

kindOf :: TypingContext -> Type -> Kind
kindOf _   TyTop          = KProper
kindOf ctx (TyVar i size) = kindOf ctx bound
  where bound = indexToTyBound ctx i size
kindOf ctx (TyAbs x k1 ty2) = KArrow k1 k2
 where
  k2   = kindOf ctx' ty2
  ctx' = TyAsmSubtype x (topKind k1) : ctx
kindOf ctx (TyApp ty1 ty2) = case k1 of
  KArrow k11 k12 | k11 == k2 -> k12
  _                          -> throw (ErrKArrowRequired ctx k1)
 where
  k1 = kindOf ctx ty1
  k2 = kindOf ctx ty2
kindOf ctx (TyArrow ty1 ty2) = case (k1, k2) of
  (KProper, KProper) -> KProper
  (KProper, _      ) -> throw (ErrKindMismatch ctx KProper k2)
  _                  -> throw (ErrKindMismatch ctx KProper k1)
 where
  k1 = kindOf ctx ty1
  k2 = kindOf ctx ty2
kindOf ctx (TyAll x b ty) = case kindOf ctx' ty of
  KProper -> KProper
  k       -> throw (ErrKindMismatch ctx KProper k)
  where ctx' = TyAsmSubtype x b : ctx
