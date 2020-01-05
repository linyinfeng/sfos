module Sfos.Typing
  ( typeOf
  , parallelReduce
  , typeEq
  , subtype
  )
where

import           Sfos.Syntax
import           Sfos.Support
import           Sfos.Kinding
import           Sfos.Error
import           Control.Exception

parallelReduce :: Type -> Type
parallelReduce = pr
 where
  pr (TyVar i size)    = TyVar i size
  pr TyTop             = TyTop
  pr (TyArrow ty1 ty2) = TyArrow (pr ty1) (pr ty2)
  pr (TyAll x b ty   ) = TyAll x (pr b) (pr ty)
  pr (TyAbs x k ty   ) = TyAbs x k (pr ty)
  pr (TyApp ty1 ty2) =
    let ty1' = pr ty1
        ty2' = pr ty2
    in  case ty1' of
          TyAbs _ _ ty12 -> pr (tySubstTop ty2' ty12)
          _              -> TyApp ty1' ty2'

typeEq :: Type -> Type -> Bool
typeEq ty1 ty2 = alphaEq ty1' ty2'
 where
  ty1' = parallelReduce ty1
  ty2' = parallelReduce ty2

subtype :: TypingContext -> Type -> Type -> Bool
subtype ctx ty1 ty2 = typeEq ty1 ty2 || subtype' ctx ty1' ty2'
 where
  ty1' = parallelReduce ty1
  ty2' = parallelReduce ty2

subtype' :: TypingContext -> Type -> Type -> Bool
-- the case i1 == i2 and size1 == size2 is covered by function subtype
subtype' ctx (TyVar i size) ty2   = subtype ctx (indexToTyBound ctx i size) ty2
subtype' _   _              TyTop = True
subtype' ctx (TyArrow ty11 ty12) (TyArrow ty21 ty22) =
  subtype' ctx ty21 ty11 && subtype' ctx ty12 ty22
subtype' ctx (TyAll x1 b1 ty1) (TyAll _ b2 ty2) = -- kernel version
  typeEq b1 b2 && hasKind (kindOf ctx b1) && subtype' ctx' ty1 ty2
 where
  ctx' = TyAsmSubtype x1 b1 : ctx
  hasKind KProper        = True
  hasKind (KArrow k1 k2) = hasKind k1 && hasKind k2
subtype' ctx (TyAbs x1 k1 ty1) (TyAbs _ k2 ty2) =
  k1 == k2 && subtype' ctx' ty1 ty2
  where ctx' = TyAsmSubtype x1 (topKind k1) : ctx
subtype' ctx (TyApp ty11 ty12) (TyApp ty21 ty22) =
  typeEq ty12 ty22 && subtype' ctx ty11 ty21 -- TODO: check this
subtype' _ _ _ = False

typeOf :: TypingContext -> Term -> Type
typeOf ctx (TmVar i size) = indexToTmType ctx i size
typeOf ctx (TmAbs x ty t) = if k == KProper
  then TyArrow ty (tyShift (-1) (typeOf ctx' t))
  else throw (ErrKindMismatch ctx KProper k)
 where
  k    = kindOf ctx ty
  ctx' = TyAsmHasType x ty : ctx
typeOf ctx (TmApp t1 t2) = case ty1 of
  TyArrow ty11 ty12 ->
    if subtype ctx ty2 ty11 then ty12 else throw (ErrNotSubtype ctx ty11 ty2)
  _ -> throw (ErrTyArrowRequired ctx ty1)
 where
  ty1 = parallelReduce $ typeOf ctx t1 -- require the shape of ty1
  ty2 = typeOf ctx t2
typeOf ctx (TmTyAbs x b t) = TyAll x b (typeOf ctx' t)
  where ctx' = TyAsmSubtype x b : ctx
typeOf ctx (TmTyApp t1 ty2) = case ty1 of
  TyAll _ b11 ty12 -> if subtype ctx ty2 b11
    then tySubstTop ty2 ty12
    else throw (ErrNotSubtype ctx b11 ty2)
  _ -> throw (ErrTyAllRequired ctx ty1)
  where ty1 = parallelReduce $ typeOf ctx t1 -- require the shape of ty1
typeOf ctx (TmLet x t1 t2) = tyShift (-1) (typeOf ctx' t2)
 where
  ty1  = typeOf ctx t1
  ctx' = TyAsmHasType x ty1 : ctx
typeOf ctx (TmTyLet _ ty1 t2) = typeOf ctx t2' where t2' = tyTmSubstTop ty1 t2
