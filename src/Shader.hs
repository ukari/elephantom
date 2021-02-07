{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Shader
  ( ShaderUniform (..)
  , ShaderInputVertex (..)
  , Texture (..)
  )
  where

import Vulkan.Core10.DescriptorSet (DescriptorSetLayoutCreateInfo (..))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType (..))
import Vulkan.Core10.Enums.Format (Format (..))
import Vulkan.Core10.Pipeline (PipelineVertexInputStateCreateInfo (..),  VertexInputBindingDescription (..), VertexInputAttributeDescription (..))

import Linear (V2 (..), V4 (..), M44)
import Data.Text (Text (..), pack , unpack)
import Data.Vector (Vector)

import Text.InterpolatedString.QM (qnb)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import qualified Language.GLSL.Parser as GLSL
import Language.GLSL.Syntax
import Language.Haskell.TH.Syntax

import Data.Functor.Foldable (cata, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Semigroup (Semigroup (..))
import Data.Map.Strict ((!), Map (..), singleton, insert, union)

import Control.Applicative (liftA2)
import Control.Monad (join)

import Offset
import Orphan
import qualified Language.GLSL.Compilation as GLSLC

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V4 Float)
  } deriving (Generic, GStorable)

makeOffset ''ShaderInputVertex

data ShaderUniform = ShaderUniform
  { model :: !(M44 Float)
  , view :: !(M44 Float)
  , proj :: !(M44 Float)
  } deriving (Generic, GStorable)

makeOffset ''ShaderUniform

data Texture = Texture
  { position :: !(V2 Float)
  , color :: !(V4 Float)
  , texCoord :: !(V2 Float)
  } deriving (Generic, GStorable)

makeOffset ''Texture

newtype Binding = Binding Int
  deriving (Show)

newtype Set = Set Int
  deriving (Show)

newtype Location = Location Int
  deriving (Show)

data UniformLayout = UniformLayout
  { binding :: !Binding
  , set :: !Set
  , type' :: !DescriptorType
  } deriving (Show)

-- data VertexInputState = VertexInputState

data VertexAttribute = VertexAttribute
  { location :: !Location
  , offset :: !Int
  , format :: !Format
  } deriving (Show)

data ShaderInfo = ShaderInfo
  { uniformLayouts :: !(Vector UniformLayout)
  , vertexAttributes :: !(Vector VertexAttribute)
  } deriving (Show)

foldMap makeBaseFunctor
  [ ''TranslationUnit
  , ''ExternalDeclaration
  , ''Declaration
  , ''InvariantOrType
  , ''FullType
  , ''TypeSpecifier
  , ''TypeQualifier
  , ''LayoutQualifier
  , ''LayoutQualifierId
  , ''Expr 
  ]

-- https://www.haskell.org/ghc/blog/20190728-free-variable-traversals.html
type Const = (String, Int)
type ConstMap = Map Const Expr

data ConstPrim
  = IntPrim Integer
  | FloatPrim Float
  | BoolPrim Bool

type Consts = Map Const ConstPrim
type Context = Map Const Int

newtype C = C
  { runC :: Consts
         -> Context
         -> Context
  }

-- newtype S = S
--   { runContext :: Context
--                -> ShaderInfo
--                -> ShaderInfo
--   }

evalExpr :: ConstMap -> Expr -> ConstPrim
evalExpr consts = \case
  Variable var -> undefined

newtype CV = CV
  { runCV :: ConstMap -- acc
          -> ConstMap -- result
  }

instance Semigroup CV where
  cv1 <> cv2 = CV $ \acc -> runCV cv1 (runCV cv2 acc)

instance Monoid CV where
  mempty = CV id

bindConst :: Const -> Expr -> CV -> CV
bindConst con expr cv = CV $ \acc -> runCV cv (insert con expr acc) 

toConstMap :: CV -> ConstMap
toConstMap cv = runCV cv mempty

globalConsts :: TranslationUnit -> ConstMap
globalConsts = \case
  TranslationUnit xs -> toConstMap $ foldMap constExternalDeclaration xs

constExternalDeclaration :: ExternalDeclaration -> CV
constExternalDeclaration = \case
  Declaration decl -> constDeclaration decl
  _ -> mempty

constDeclaration :: Declaration -> CV
constDeclaration = \case
  InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Const)) _)) decs -> foldMap constInitDeclarator decs
  _ -> mempty

constInitDeclarator :: InitDeclarator -> CV
constInitDeclarator = \case
  InitDecl name (Just (Just (IntConstant _ arr))) (Just expr) -> foldMap (\idx -> bindConst (name, idx) expr mempty) [0 .. (fromIntegral arr)]
  InitDecl name _ (Just expr) -> bindConst (name, 0) expr mempty
  _ -> mempty

test = globalConsts <$> (GLSL.parse . unpack $ testShaderStr)

globalConsts' :: TranslationUnit -> ConstMap
globalConsts' = \case
  TranslationUnit xs -> toConstMap $ foldMap constExternalDeclaration xs

constExternalDeclaration' :: ExternalDeclaration -> ConstMap
constExternalDeclaration' = \case
  Declaration decl -> constDeclaration' decl
  _ -> mempty

constDeclaration' :: Declaration -> ConstMap
constDeclaration' = \case
  InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Const)) _)) decs -> foldMap constInitDeclarator' decs
  _ -> mempty

constInitDeclarator' :: InitDeclarator -> ConstMap
constInitDeclarator' = \case
  InitDecl name (Just (Just (IntConstant _ arr))) (Just expr) -> foldMap (\idx -> insert (name, idx) expr mempty) [0 .. (fromIntegral arr)]
  InitDecl name _ (Just expr) -> insert (name, 0) expr mempty
  _ -> mempty

test' = globalConsts' <$> (GLSL.parse . unpack $ testShaderStr)



interp :: TranslationUnit -> Int
interp = cata $ \case
  TranslationUnitF xs -> sum $ interpExternalDeclaration <$> xs

interpExternalDeclaration :: ExternalDeclaration -> Int
interpExternalDeclaration = cata $ \case
  DeclarationF x -> interpDeclaration x
  _ -> 0

interpDeclaration :: Declaration -> Int
interpDeclaration = cata $ \case
  BlockF layout utype _fields arr -> interpTypeQualifier layout
  TQF layout -> interpTypeQualifier layout
  InitDeclarationF dec arrs -> length arrs
  _ -> 0

interpInvariantOrType :: InvariantOrType -> Int
interpInvariantOrType = cata $ \case
  TypeDeclaratorF tdec -> 1
  _ -> 0

interpFullType :: FullType -> Int
interpFullType = cata $ \case
  FullTypeF layout tspec -> 1

interpTypeQualifier :: TypeQualifier -> Int
interpTypeQualifier = cata $ \case
  TypeQualLayF x y -> 1
  _ -> 0

interpLayoutQualifierId :: LayoutQualifierId -> Int
interpLayoutQualifierId = cata $ \case
  LayoutQualIdF tname (Just expr) -> interpExpr expr
  

interpExpr :: Expr -> Int
interpExpr = cata $ \case
  VariableF _ -> error "not support variable"
  IntConstantF _ i -> fromIntegral i
  _ -> error "not support"

foo = TranslationUnit
  [ Declaration
    ( Block
      ( TypeQualLay
        ( Layout
          [ LayoutQualId "set" (Just (IntConstant Decimal 0))
          , LayoutQualId "binding" (Just (IntConstant Decimal 0))])
        ( Just Uniform))
      "UniformBufferObject"
      [ Field Nothing
        ( TypeSpec Nothing (TypeSpecNoPrecision Mat4 Nothing))
        [ StructDeclarator "model" Nothing]
      , Field Nothing
        ( TypeSpec Nothing (TypeSpecNoPrecision Mat4 Nothing))
        [ StructDeclarator "view" Nothing]
      , Field Nothing
        ( TypeSpec Nothing (TypeSpecNoPrecision Mat4 Nothing))
        [ StructDeclarator "proj" Nothing]]
      ( Just ( "ubo"
             , Just (Just (IntConstant Decimal 2)))))
  , Declaration
    ( InitDeclaration
      ( TypeDeclarator
        ( FullType
          ( Just
            ( TypeQualLay
              ( Layout
                [ LayoutQualId "location" (Just (IntConstant Decimal 0))] )
              ( Just In)))
          ( TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))))
      [ InitDecl "inPosition" Nothing Nothing])
  , Declaration
    ( InitDeclaration
      ( TypeDeclarator
        ( FullType
          ( Just
            ( TypeQualLay
              ( Layout
                [ LayoutQualId "location" (Just (IntConstant Decimal 1))])
              ( Just In)))
          ( TypeSpec Nothing (TypeSpecNoPrecision Vec4 Nothing))))
      [ InitDecl "inColor" (Just (Just (IntConstant Decimal 3))) Nothing])
  , Declaration
    ( InitDeclaration
      ( TypeDeclarator
        ( FullType
          ( Just
            ( TypeQualLay
              ( Layout
                [ LayoutQualId "location" (Just (IntConstant Decimal 0))])
              ( Just Out)))
          ( TypeSpec Nothing (TypeSpecNoPrecision Vec4 Nothing))))
      [ InitDecl "fragColor" Nothing Nothing])
  , FunctionDefinition
    ( FuncProt
      ( FullType Nothing
        ( TypeSpec Nothing
          ( TypeSpecNoPrecision Void Nothing))) "main" [])
    ( Compound
      [ ExpressionStatement
        ( Just
          ( Equal
            ( Variable "gl_Position")
            ( Mul
              ( Mul
                ( Mul
                  ( FieldSelection (Variable "ubo") "proj")
                  ( FieldSelection (Variable "ubo") "view"))
                ( FieldSelection (Variable "ubo") "model"))
              ( FunctionCall
                ( FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec4 Nothing)))
                ( Params [Variable "inPosition",FloatConstant 0.0,FloatConstant 1.0])))))
      , ExpressionStatement (Just (Equal (Variable "fragColor") (Variable "inColor")))])]

testShaderStr :: Text
testShaderStr = [qnb|

  layout(set = 0, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo[2];

  const int a = 2+2;
  const bool b = true;
  const int c = b ? 1 : a + 1;
  //const int d[3] = {4,5,6}; // not support
  const int e[3] = int[](7,8,9);
  const int g = e[0];

  layout(location = 0) in vec2 inPosition;
  layout(location = 1) in vec4 inColor[a];
  
  layout(location = 1) out vec4 fragColor;

  void main() {
    gl_Position = ubo[0].proj * ubo[0].view * ubo[0].model * vec4(inPosition, 0.0, 1.0);
    fragColor = inColor[1];
  }
  |]

testShaderStr2 :: String
testShaderStr2 = [qnb|
  #version 450
  #pragma STDGL invariant(all)
  #extension GL_ARB_separate_shader_objects : enable
  layout(binding = 1) uniform sampler2D texSampler;

  layout(location = 0) in vec4 fragColor;
  layout(location = 1) in vec2 fragTexCoord;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = texture(texSampler, fragTexCoord); // for image use texSampler, for shape created by rasterfic use fragColor
  }
  |]
