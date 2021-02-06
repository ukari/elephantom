module Orphan
  (
  ) where

--import Data.Semigroup
import Language.Haskell.TH.Syntax
-- import Control.Monad (join)
import Control.Applicative (liftA2)

-- https://gitlab.haskell.org/ghc/ghc/-/blob/640a3ece333d1b0d0af8f353c3e1df9dd0cb9ef3/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L215
-- should be removed when use template-haskell >= 2.17.0.0 
instance Semigroup a => Semigroup (Q a) where
  --(<>) a b = (pure . join =<<) . sequence $ [a, b]
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Q a) where
  mempty = pure mempty
