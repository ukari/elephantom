{-# LANGUAGE OverloadedStrings #-}

module Elephantom.Application
  ( Application (..)
  , defaultApplication
  ) where

import Data.Text (Text)

data Application = Application
  { vkDebug :: !Bool
  , title :: !Text
  } deriving (Show)

defaultApplication :: Application
defaultApplication = Application
  { vkDebug = True
  , title = "test测试"
  }
