{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
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
import Language.Haskell.TH.Syntax (Lift)

import Type.Reflection (SomeTypeRep, splitApps)

import Foreign.Storable (Storable (sizeOf))
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs, dataTypeName, typeOf, typeRepFingerprint)
import Data.List (elemIndex)

import Linear (V2 (..), V3 (..))
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

offsetOf :: (Data a, Storable a) => a -> String -> Q Exp
offsetOf s field = do
  let typename = dataTypeName . dataTypeOf $ s
  let [fs] = fields s
  let labels = map (varE . mkName) $ fs
  x <- newName "x"
  let normals = listE $ map (appE (lamE [varP x] (infixApp (varE x) (varE '($)) (nil s)))) (map (infixApp (infixApp fromInt dot sizeof) dot) labels)
  case elemIndex field fs of
    Just idx -> [|sum $ take idx $(normals) :: Int|]
    Nothing -> error $ "field '" <> field <> "' not found in " <> typename
  where
    sizeof = varE 'sizeOf
    dot = varE '(.)
    fromInt = varE 'fromIntegral
    fields :: Data a => a -> [[String]]
    fields = map constrFields . dataTypeConstrs . dataTypeOf
    nil :: a -> Q Exp
    nil _ = sigE (varE 'undefined) (varT . mkName $ "a")

offsetOf' :: (Data a, Storable a) => a -> Q Exp
offsetOf' s = do
  let typename = dataTypeName . dataTypeOf $ s
  let [fs] = fields s
  let labels = map (varE . mkName) $ fs
  x <- newName "x"
  let normals = listE $ map (appE (lamE [varP x] (infixApp (varE x) (varE '($)) (nil s)))) (map (infixApp (infixApp fromInt dot sizeof) dot) labels)
  [|\field -> case elemIndex field fs of
    Just idx -> sum $ take idx $(normals) :: Int
    Nothing -> error $ "field '" <> field <> "' not found in " <> typename
   |]
  where
    sizeof = varE 'sizeOf
    dot = varE '(.)
    fromInt = varE 'fromIntegral
    fields :: Data a => a -> [[String]]
    fields = map constrFields . dataTypeConstrs . dataTypeOf
    nil :: a -> Q Exp
    nil _ = sigE (varE 'undefined) (varT . mkName $ "a")

data Bar = Bar Int Int deriving (Generic, GStorable, Data)

data Foo = FooA
  { x :: Int
  , y :: Int
  } | FooB
  { z :: Int
  } deriving (Generic, GStorable)

