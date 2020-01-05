module Sfos.Syntax
  ( Term(..)
  , Type(..)
  , Kind(..)
  , TypingAssumption(..)
  , TypingContext
  , value
  , topKind
  , AlphaEq(..)
  )
where

data Term = TmVar Int Int
          | TmAbs String Type Term
          | TmApp Term Term
          | TmTyAbs String Type Term
          | TmTyApp Term Type
          | TmLet String Term Term
          | TmTyLet String Type Term
          deriving Show

data Type = TyTop
          | TyVar Int Int
          | TyArrow Type Type
          | TyAll String Type Type
          | TyAbs String Kind Type
          | TyApp Type Type
          deriving Show

data Kind = KProper
          | KArrow Kind Kind
          deriving (Show, Eq)

data TypingAssumption = TyAsmHasType String Type
                      | TyAsmSubtype String Type
                      deriving Show

type TypingContext = [TypingAssumption]

value :: Term -> Bool
value (TmVar _ _)   = False
value TmAbs{}       = True
value (TmApp _ _)   = False
value TmTyAbs{}     = True
value (TmTyApp _ _) = False
value TmLet{}       = False
value TmTyLet{}     = False

-- Top[K]
topKind :: Kind -> Type
topKind KProper        = TyTop
topKind (KArrow k1 k2) = TyAbs "_" k1 (topKind k2)

class AlphaEq a where
  alphaEq :: a -> a -> Bool
  alphaEq x y = not (alphaNeq x y)
  alphaNeq :: a -> a -> Bool
  alphaNeq x y = not (alphaEq x y)
  {-# MINIMAL alphaEq | alphaNeq #-}

instance AlphaEq Term where
  alphaEq (TmVar i1 size1 ) (TmVar i2 size2 ) = i1 == i2 && size1 == size2
  alphaEq (TmAbs _ ty1 t1 ) (TmAbs _ ty2 t2 ) = alphaEq ty1 ty2 && alphaEq t1 t2
  alphaEq (TmApp t11 t12) (TmApp t21 t22) = alphaEq t11 t21 && alphaEq t12 t22
  alphaEq (TmTyAbs _ b1 t1) (TmTyAbs _ b2 t2) = alphaEq b1 b2 && alphaEq t1 t2
  alphaEq (TmTyApp t1 ty1 ) (TmTyApp t2 ty2 ) = alphaEq t1 t2 && alphaEq ty1 ty2
  alphaEq (TmLet _ t11 t12) (TmLet _ t21 t22) =
    alphaEq t11 t21 && alphaEq t12 t22
  alphaEq (TmTyLet _ ty11 t12) (TmTyLet _ ty21 t22) =
    alphaEq ty11 ty21 && alphaEq t12 t22
  alphaEq _ _ = False

instance AlphaEq Type where
  alphaEq TyTop            TyTop            = True
  alphaEq (TyVar i1 size1) (TyVar i2 size2) = i1 == i2 && size1 == size2
  alphaEq (TyArrow ty11 ty12) (TyArrow ty21 ty22) =
    alphaEq ty11 ty21 && alphaEq ty12 ty22
  alphaEq (TyAll _ b1 ty1) (TyAll _ b2 ty2) = alphaEq b1 b2 && alphaEq ty1 ty2
  alphaEq (TyAbs _ k1 ty1) (TyAbs _ k2 ty2) = k1 == k2 && alphaEq ty1 ty2
  alphaEq (TyApp ty11 ty12) (TyApp ty21 ty22) =
    alphaEq ty11 ty21 && alphaEq ty12 ty22
  alphaEq _ _ = False
