{-# LANGUAGE FlexibleInstances #-}

module Sfos.Show
  ( WithCtx(..)
  )
where

import           Sfos.Syntax
import           Sfos.Support

data WithCtx a = WithCtx TypingContext a

instance Show (WithCtx Term) where
  showsPrec d t = case t of
    WithCtx ctx (TmVar i size) -> showString $ indexToTmName ctx i size
    WithCtx ctx (TmAbs x ty t2) ->
      showParen (d > 0)
        $ showString "\x03BB"
        . showString x
        . showString " : "
        . shows (WithCtx ctx ty)
        . showString ". "
        . shows (WithCtx ctx' t2)
      where ctx' = TyAsmHasType x ty : ctx
    WithCtx ctx (TmApp t1 t2) ->
      showParen (d > 10)
        $ showsPrec 10 (WithCtx ctx t1)
        . showString " "
        . showsPrec 11 (WithCtx ctx t2)
    WithCtx ctx (TmTyAbs x ty t2) ->
      showParen (d > 0)
        $ showString "\x03BB"
        . showString x
        . showString " <: "
        . shows (WithCtx ctx ty)
        . showString ". "
        . shows (WithCtx ctx' t2)
      where ctx' = TyAsmSubtype x ty : ctx
    WithCtx ctx (TmTyApp t1 ty) ->
      showParen (d > 10)
        $ showsPrec 10 (WithCtx ctx t1)
        . showString " ["
        . shows (WithCtx ctx ty)
        . showString "]"
    WithCtx ctx (TmLet x t1 t2) ->
      showParen (d > 0)
        $ showString "let "
        . showString x
        . showString " = "
        . shows (WithCtx ctx t1)
        . showString " in\n"
        . shows (WithCtx ctx' t2)
      where ctx' = TyAsmHasType x TyTop : ctx -- type not important
    WithCtx ctx (TmTyLet x ty1 t2) ->
      showParen (d > 0)
        $ showString "let "
        . showString x
        . showString " = "
        . shows (WithCtx ctx ty1)
        . showString " in\n"
        . shows (WithCtx ctx' t2)
      where ctx' = TyAsmSubtype x TyTop : ctx -- type not important

instance Show (WithCtx Type) where
  showsPrec d ty = case ty of
    WithCtx _   TyTop          -> showString "Top"
    WithCtx ctx (TyVar i size) -> showString $ indexToTyName ctx i size
    WithCtx ctx (TyArrow ty1 ty2) ->
      showParen (d > 1)
        $ showsPrec 2 (WithCtx ctx ty1)
        . showString " -> "
        . showsPrec 1 (WithCtx ctx ty2)
    WithCtx ctx (TyAll x b ty2) ->
      showParen (d > 0)
        $ showString "\x2200"
        . showString x
        . showString " <: "
        . shows (WithCtx ctx b)
        . showString ". "
        . shows (WithCtx ctx' ty2)
      where ctx' = TyAsmSubtype x b : ctx
    WithCtx ctx (TyAbs x k ty2) ->
      showParen (d > 0)
        $ showString "\x03BB"
        . showString x
        . showString " :: "
        . shows (WithCtx ctx k)
        . showString ". "
        . shows (WithCtx ctx' ty2)
      where ctx' = TyAsmSubtype x (topKind k) : ctx
    WithCtx ctx (TyApp ty1 ty2) ->
      showParen (d > 10)
        $ showsPrec 10 (WithCtx ctx ty1)
        . showString " "
        . showsPrec 11 (WithCtx ctx ty2)

instance Show (WithCtx Kind) where
  showsPrec d k = case k of
    WithCtx _ KProper -> showString "*"
    WithCtx ctx (KArrow k1 k2) ->
      showParen (d > 0) $ shows (WithCtx ctx k1) . showString " => " . showsPrec
        1
        (WithCtx ctx k2)

instance Show (WithCtx TypingContext) where
  show (WithCtx _ ((TyAsmHasType x ty) : ctx')) =
    show (WithCtx ctx' ctx') ++ ", " ++ x ++ " : " ++ show (WithCtx ctx' ty)
  show (WithCtx _ ((TyAsmSubtype x ty) : ctx')) =
    show (WithCtx ctx' ctx') ++ ", " ++ x ++ " <: " ++ show (WithCtx ctx' ty)
  show (WithCtx _ []) = "\x2205"
