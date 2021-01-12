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
  ( -- foo
  -- , foo2
  -- , foo3
  -- --, field
  -- , Vertex2 (..)
  --,
  module Tmp
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)

import Type.Reflection (SomeTypeRep, splitApps)

import Foreign.Storable (Storable (sizeOf))
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs, dataTypeName, typeOf, typeRepFingerprint)
import Data.List (elemIndex)

import Linear (V2 (..), V3 (..))
import GHC.Generics ((:*:) (..), Generic (..))
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)

import qualified GHC.Err as Err

nil :: a -> Q Exp
nil _ = [|undefined :: a|]

nil' :: a -> Q Exp
nil' _ = sigE (varE 'undefined) (varT (mkName "a"))

foo :: Q Exp
foo = do
  let x = mkName "x"
  lamE [varP x] (varE x)

foo2 = conE $ mkName "Vertex2" :: ExpQ

foo3 = varE (mkName "sizeOf") 

fields :: Data a => a -> [[String]]
fields = map constrFields . dataTypeConstrs . dataTypeOf

test = [| \(x) ->
  case x of
    Just y -> True
    Nothing -> fail "fuck"
  |]

test2 = [| map ($ (undefined::ShaderInputVertex)) [fromIntegral . sizeOf . inPosition, fromIntegral . sizeOf . inColor]
         |]
        
test3 = [| map (($ (undefined::ShaderInputVertex) ) (\x -> (fromIntegral . sizeOf) . x)) [inPosition, inColor]
         |]

test4 = [| ((\x -> (x $ (undefined::ShaderInputVertex) )) ((fromIntegral . sizeOf) .))
         |]
        
-- test4 = [| map ((fromIntegral . sizeOf) .) [inPosition, inColor]
--          |]

offsetOf :: (Data a, Storable a) => a -> String -> Q Exp
offsetOf x1 field = do
  let typename = dataTypeName . dataTypeOf $ x1 
  let [fs] = fields x1
  --let labels = listE . map stringE $ fs
  let labels = map (varE . mkName) $ fs
  
  x <- newName "x"
  --let normals = undefined--listE (map (appE (appE (infixE Nothing (dyn "$") (Just (sigE (dyn "undefined") (conT (mkName "a"))))) (infixApp (infixApp fromInt dot sizeof) dot)) labels))
  let normals = listE $ map (appE (lamE [varP x] (infixApp (varE x) (dyn "$") (sigE (dyn "undefined") (varT (mkName "a")))))) (map (infixApp (infixApp fromInt dot sizeof) dot) labels)
  case elemIndex field fs of
    Just idx -> [|sum $ take idx $(normals) :: Int|]
    Nothing -> fail $ "field '" <> field <> "' not found in " <> typename
  --let total =  sumSize labels
  -- let prevsLabels = map (varE . mkName) $ takeWhile (/= fieldname) fs
  --let prevs = sumSize prevsLabels
  --prevs
  --field <- newName "field"
  
  --i <- newName "i"
  --appE (infixApp sizeof dot (varE . mkName $ head fs)) (nil (undefined::a))
  --(dyn "takeWhile")
--  --lam1E (sigP (varP field) (appT (varT . mkName $ "Eq") (varT . mkName $ "String"))) (appE (dyn "/=") (varE field))

  --appE (dyn "/=") (litE $ StringL "fuck")
  
  -- lam1E (varP field)
  --   (caseE (infixApp (varE field) (dyn "elemIndex") labels)
  --    [
  --    ])
  --   (appE
  --     (appE (dyn "takeWhile")
  --       (lam1E (varP x)
  --        (infixApp (varE x) (dyn "/=") (varE field))))
  --     labels)
  -- fail "fuck"
  -- if length fs == 0
  --   then
  -- [| case elemIndex field fs of
  --    Just idx -> sum $ take idx $(normals) ::Int -- undefined--(map (($ (undefined::ShaderInputVertex)) (fromIntegral . sizeOf) .) $(labels))  -- map ($ (undefined::a)) (map ((fromIntegral . sizeOf) .) (take idx $(labels)))
  --    Nothing -> error $ "field '" <> field <> "' not found in " <> typename
  --  |]
    -- else
    -- [|\fieldIndex -> do
    --  case fieldIndex < length fs of
    --    True -> sum $ take fieldIndex 

    --  |]
  --normals
  --labels
  --lam1E (sigP (varP field) (varT ((mkName "String")))) (appE (varE field))
  
  --sum . map (infixApp sizeof dot) $ prevsLabels
  --appE (infixApp sizeof dot (head labels)) (varE $ mkName "x1")
  --(uInfixE dot (sizeof)) x
  -- varE $ mkName "x1"
  where
    sizeof :: Q Exp
    sizeof = varE . mkName $ "sizeOf"
    dot :: Q Exp
    dot = varE . mkName $ "."
    sumSize :: [Q Exp] -> Q Exp
    sumSize = sum . map (infixApp (infixApp fromInt dot sizeof) dot )
    fromInt = varE . mkName $ "fromIntegral"
    fromLabels xs = map (infixApp (infixApp fromInt dot sizeof) dot) xs
    --fromSizeof = uInfixE dot fromInt sizeof


data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V3 Float)
  } deriving (Generic, GStorable, Data)

data Bar = Bar Int Int deriving (Generic, GStorable, Data)
