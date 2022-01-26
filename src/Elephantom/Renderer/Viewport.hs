{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Elephantom.Renderer.Viewport
  {-# DEPRECATED "make viewport directly from extent instead" #-}
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

-- note: make viewport with aspect ratio and center align seems not good for fragment shader coord
makeViewport :: Application -> Extent2D -> Viewport
makeViewport app (Extent2D windowWidth windowHeight) = do
  let (width, height) = calculateViewport (Application.width app) (Application.height app) (fromIntegral windowWidth) (fromIntegral windowHeight)
  let x = fromIntegral @Int . floor @Rational $ ((fromIntegral windowWidth - fromIntegral width) / 2)
  let y = fromIntegral @Int . floor @Rational $ ((fromIntegral windowHeight - fromIntegral height) / 2)
  Viewport
    { x = x
    , y = y
    , width = fromIntegral width
    , height = fromIntegral height
    , minDepth = 0
    , maxDepth = 1
    }
