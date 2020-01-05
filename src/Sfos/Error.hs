module Sfos.Error
  ( Error(..)
  )
where

import           Sfos.Syntax
import           Control.Exception

data Error = ErrKindMismatch TypingContext Kind Kind
           | ErrNotSubtype TypingContext Type Type
           | ErrTyArrowRequired TypingContext Type
           | ErrTyAllRequired TypingContext Type
           | ErrKArrowRequired TypingContext Kind

instance Exception Error

instance Show Error where
  show (ErrKindMismatch _ k1 k2) =
    "Kind mismatch, required: " ++ show k1 ++ ", actual: " ++ show k2
  show (ErrNotSubtype _ ty1 ty2) =
    "Not subtype, required: " ++ show ty1 ++ ", actual: " ++ show ty2
  show (ErrTyArrowRequired _ ty) = "Require an arrow type, actual: " ++ show ty
  show (ErrTyAllRequired   _ ty) = "Require an all type, actual: " ++ show ty
  show (ErrKArrowRequired  _ k ) = "Require an arrow kind, actual: " ++ show k
