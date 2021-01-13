{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}


module Tmp
  ( module Tmp
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Type.Reflection (SomeTypeRep, splitApps)

import Foreign.Storable (Storable (sizeOf))
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs, dataTypeName, typeOf, typeRepFingerprint)
import Data.List (elemIndex)
import Data.Either (lefts, rights)
import Linear (V2 (..), V3 (..))
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

data OffsetSelect
  = Record String
  | Normal Int
  deriving (Show)


offsetOfN :: Name -> Q Exp
offsetOfN name = do
  TyConI (DataD _ fullName _ _ cons _)<- reify name
  let fieldConTss = map fConT cons 
      isSumtype = length fieldConTss > 1
      fullname = show fullName
  if isSumtype
    then fail $ "not support sumtype for '" <> fullname <> "'" 
    else do
    let [fieldConTs] = fieldConTss
        normals = lefts fieldConTs :: [Type]
        records = rights fieldConTs :: [(Name, Type)]
        isNormal = length normals /= 0 :: Bool
        isRecord = length records /= 0 :: Bool
        sizes | isNormal = map (trans . nil . pure) $ normals :: [Q Exp]
              | isRecord = map (trans . nil . pure . snd) $ records
              | otherwise = fail "only support c struct like storable types (shouldn't run here)"
        sizesE = listE sizes
        normalsLength = length sizes
        rnames = map (nameBase . fst) records
    [|\case
       Record fieldstr -> case (elemIndex fieldstr rnames) of
         Just idx -> sum $ take idx $(sizesE)
         Nothing -> error $ "field '" <> fieldstr <> "' is not found in '" <> fullname <> "'"
       Normal idx -> case idx < normalsLength of
         True -> sum $ take idx $(sizesE)
         False -> error $ "field with index '" <> show idx <> "' is not found in '" <> fullname <> "'"
     |]
  where
    fConT :: Con -> [Either (Type) (Name, Type)]
    fConT (NormalC _ iFields) = map (Left . fStrictType) iFields
    fConT (RecC _ sFields) = map (Right . fVarStrictType) sFields
    fConT _ = fail "only support c struct like storable types"
    fStrictType :: StrictType -> Type
    fStrictType (_, tp) = tp
    fVarStrictType :: VarStrictType -> (Name, Type)
    fVarStrictType (fieldname, _, tp) = (fieldname, tp)
    nil :: Q Type -> Q Exp
    nil t = sigE (varE 'undefined) t
    sizeof = varE 'sizeOf
    dot = varE '(.)
    app = varE '($)
    fromInt = varE 'fromIntegral
    trans :: Q Exp -> Q Exp
    trans = infixApp (infixApp fromInt dot sizeof) app

data Bar = Bar Int Int deriving (Generic, GStorable, Data)

data Foo = FooA
  { x :: Int
  , y :: Int
  } | FooB
  { z :: Int
  } | FooC Int
  deriving (Generic, GStorable)

