{-# LANGUAGE OverloadedStrings #-}

module Elephantom.Application
  ( Application (..)
  , defaultApplication
  ) where

import Data.Text (Text)

data Application = Application
  { vkDebug :: !Bool
  , title :: !Text
  , fps :: !Int
  , inputRate :: !Int
  , width :: !Int
  , height :: !Int
  } deriving (Show)

defaultApplication :: Application
defaultApplication = Application
  { vkDebug = True
  , title = "test测试"
  , fps = 1
  , inputRate = 120
  , width = 500
  , height = 500
  }
