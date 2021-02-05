{-# LANGUAGE FlexibleInstances #-}

module Orphan
  (
  ) where

--import Data.Semigroup
import Language.Haskell.TH.Syntax
--import Control.Monad (join)
import Control.Applicative (liftA2)

instance Semigroup (Q [a]) where
  --(<>) a b = (pure . join =<<) . sequence $ [a, b]
  (<>) = liftA2 (<>)

instance Monoid (Q [a]) where
  mempty = pure []
