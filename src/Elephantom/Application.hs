{-# LANGUAGE OverloadedStrings #-}

module Elephantom.Application
  ( Application (..)
  , defaultApplication
  ) where

import Data.Text (Text)
import Data.Word (Word8)

data Application = Application
  { vkDebug :: !Bool
  , title :: !Text
  , fps :: !Int
  , inputRate :: !Int
  , width :: !Int
  , height :: !Int
  , bgRed :: !Word8
  , bgGreen :: !Word8
  , bgBlue :: !Word8
  } deriving (Show)

defaultApplication :: Application
defaultApplication = Application
  { vkDebug = True
  , title = "test测试"
  , fps = 1
  , inputRate = 120
  , width = 500
  , height = 500
  , bgRed = 255
  , bgGreen = 255
  , bgBlue = 255
  }
