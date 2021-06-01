{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Elephantom.Renderer.Viewport
  ( calculateViewport
  , makeViewport
  ) where

import Vulkan

import Elephantom.Application (Application (..))
import qualified Elephantom.Application as Application

calculateViewport :: "appWidth" ::: Int -> "appHeight" ::: Int -> "windowWidth" ::: Int -> "windowHeight" ::: Int -> ("width" ::: Int, "height" ::: Int)
calculateViewport appWidth appHeight windowWidth windowHeight =
  if windowAspectRatio < aspectRatio 
  then
    (windowWidth, floor $ fromIntegral windowWidth / aspectRatio) 
  else
    (floor $ fromIntegral windowHeight * aspectRatio, windowHeight)
  where
    aspectRatio :: Rational
    aspectRatio = fromIntegral appWidth / fromIntegral appHeight
    windowAspectRatio :: Rational
    windowAspectRatio = fromIntegral windowWidth / fromIntegral windowHeight

makeViewport :: Application -> Extent2D -> Viewport
makeViewport app (Extent2D windowWidth windowHeight) = do
  let (width, height) = calculateViewport (Application.width app) (Application.height app) (fromIntegral windowWidth) (fromIntegral windowHeight)
  Viewport
    { x = fromIntegral @Int . floor @Rational $ ((fromIntegral windowWidth - fromIntegral width) / 2)
    , y = fromIntegral @Int . floor @Rational $ ((fromIntegral windowHeight - fromIntegral height) / 2)
    , width = fromIntegral width
    , height = fromIntegral height
    , minDepth = 0
    , maxDepth = 1  
    }
