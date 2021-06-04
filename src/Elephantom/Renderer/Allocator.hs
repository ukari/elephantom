{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.Allocator
  ( Allocator
  ) where

type Allocator m a = forall r . (m a -> (a -> m ()) -> r) -> r
