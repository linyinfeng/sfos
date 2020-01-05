module Sfos.Support
  ( tmNameToIndex
  , tyNameToIndex
  , indexToTmName
  , indexToTyName
  , indexToTmType
  , indexToTyBound
  , indexToTmAsm
  , indexToTyAsm
  , indexToAsm
  , assertCtxLength
  , tyMap
  , tmMap
  , getAsm
  , tyShiftAbove
  , tmShiftAbove
  , asmShiftAbove
  , tyShift
  , tmShift
  , asmShift
  , tySubst
  , tmSubst
  , tyTmSubst
  , tySubstTop
  , tmSubstTop
  , tyTmSubstTop
  )
where

import           Sfos.Syntax
import           Data.List
import           Text.Show.Prettyprint

tmNameToIndex :: TypingContext -> String -> Maybe Int
tmNameToIndex ctx x = findIndex match ctx
 where
  match (TyAsmHasType name _) = name == x
  match (TyAsmSubtype _    _) = False

tyNameToIndex :: TypingContext -> String -> Maybe Int
tyNameToIndex ctx x = findIndex match ctx
 where
  match (TyAsmHasType _    _) = False
  match (TyAsmSubtype name _) = name == x

indexToTmName :: TypingContext -> Int -> Int -> String
indexToTmName ctx i size = let (name, _) = indexToTmAsm ctx i size in name

indexToTyName :: TypingContext -> Int -> Int -> String
indexToTyName ctx i size = let (name, _) = indexToTyAsm ctx i size in name

indexToTmType :: TypingContext -> Int -> Int -> Type
indexToTmType ctx i size = let (_, ty) = indexToTmAsm ctx i size in ty

indexToTyBound :: TypingContext -> Int -> Int -> Type
indexToTyBound ctx i size = let (_, ty) = indexToTyAsm ctx i size in ty

indexToTmAsm :: TypingContext -> Int -> Int -> (String, Type)
indexToTmAsm ctx i size = case indexToAsm ctx i size of
  TyAsmHasType name ty -> (name, ty)
  TyAsmSubtype _ _ ->
    error $ "Index " ++ show i ++ " of typing context should be term assumption"

indexToTyAsm :: TypingContext -> Int -> Int -> (String, Type)
indexToTyAsm ctx i size = case indexToAsm ctx i size of
  TyAsmHasType _ _ ->
    error $ "Index " ++ show i ++ " of typing context should be type assumption"
  TyAsmSubtype name ty -> (name, ty)

indexToAsm :: TypingContext -> Int -> Int -> TypingAssumption
indexToAsm ctx i size = case assertCtxLength ctx size (ctx !! i) of
  TyAsmHasType x ty -> TyAsmHasType x (tyShift (i + 1) ty)
  TyAsmSubtype x b  -> TyAsmSubtype x (tyShift (i + 1) b)

assertCtxLength :: TypingContext -> Int -> a -> a
assertCtxLength ctx size = if length ctx == size
  then id
  else
    error
    $  "Context length mismatch, required: "
    ++ show size
    ++ ", actual: "
    ++ show (length ctx)
    ++ ", context: "
    ++ prettyShow ctx

getAsm :: TypingContext -> Int -> Int -> TypingAssumption
getAsm ctx i size = asmShift (i + 1) $ indexToAsm ctx i size

tyMap :: (Int -> (Int, Int) -> Type) -> Int -> Type -> Type
tyMap onVar = walk
 where
  walk _ TyTop              = TyTop
  walk c (TyVar   i   size) = onVar c (i, size)
  walk c (TyArrow ty1 ty2 ) = TyArrow (walk c ty1) (walk c ty2)
  walk c (TyAll x b ty2   ) = TyAll x (walk c b) (walk (c + 1) ty2)
  walk c (TyAbs x k ty2   ) = TyAbs x k (walk (c + 1) ty2)
  walk c (TyApp ty1 ty2   ) = TyApp (walk c ty1) (walk c ty2)

tmMap
  :: (Int -> (Int, Int) -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
tmMap onVar onType = walk
 where
  walk c (TmVar i size    ) = onVar c (i, size)
  walk c (TmAbs x ty t2   ) = TmAbs x (onType c ty) (walk (c + 1) t2)
  walk c (TmApp t1 t2     ) = TmApp (walk c t1) (walk c t2)
  walk c (TmTyAbs x b t2  ) = TmTyAbs x (onType c b) (walk (c + 1) t2)
  walk c (TmTyApp t1 ty   ) = TmTyApp (walk c t1) (onType c ty)
  walk c (TmLet   x t1  t2) = TmLet x (walk c t1) (walk (c + 1) t2)
  walk c (TmTyLet x ty1 t2) = TmTyLet x (onType c ty1) (walk (c + 1) t2)

tyShiftAbove :: Int -> Int -> Type -> Type
tyShiftAbove d = tyMap onVar
 where
  onVar c (i, size) | i >= c    = TyVar (i + d) (size + d)
                    | otherwise = TyVar i (size + d)

tmShiftAbove :: Int -> Int -> Term -> Term
tmShiftAbove d = tmMap onVar onType
 where
  onVar c (i, size) | i >= c    = TmVar (i + d) (size + d)
                    | otherwise = TmVar i (size + d)
  onType = tyShiftAbove d

tyShift :: Int -> Type -> Type
tyShift d = tyShiftAbove d 0

tmShift :: Int -> Term -> Term
tmShift d = tmShiftAbove d 0

asmShiftAbove :: Int -> Int -> TypingAssumption -> TypingAssumption
asmShiftAbove d c (TyAsmHasType x ty) = TyAsmHasType x (tyShiftAbove d c ty)
asmShiftAbove d c (TyAsmSubtype x ty) = TyAsmSubtype x (tyShiftAbove d c ty)

asmShift :: Int -> TypingAssumption -> TypingAssumption
asmShift d = asmShiftAbove d 0

tmSubst :: Int -> Term -> Term -> Term
tmSubst j s = tmMap onVar onType j
 where
  onVar c (i, size) | i == c    = tmShift i s
                    | otherwise = TmVar i size
  onType _ = id

tmSubstTop :: Term -> Term -> Term
tmSubstTop s t = tmShift (-1) (tmSubst 0 (tmShift 1 s) t)

tySubst :: Int -> Type -> Type -> Type
tySubst j s = tyMap onVar j
 where
  onVar c (i, size) | i == c    = tyShift i s
                    | otherwise = TyVar i size

tySubstTop :: Type -> Type -> Type
tySubstTop s t = tyShift (-1) (tySubst 0 (tyShift 1 s) t)

tyTmSubst :: Int -> Type -> Term -> Term
tyTmSubst j s = tmMap onVar onType j
 where
  onVar _ (i, size) = TmVar i size
  onType c = tySubst c s

tyTmSubstTop :: Type -> Term -> Term
tyTmSubstTop s t = tmShift (-1) (tyTmSubst 0 (tyShift 1 s) t)
