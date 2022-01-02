{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.Wrapper.Allocate
  ( Allocate
  ) where

type Allocate m = forall r . IO r -> (r -> IO ()) -> m r
