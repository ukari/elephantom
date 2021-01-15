{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Foreign.Storable.Offset
  ( makeOffset
  , Offset (..)
  ) where

import Language.Haskell.TH

import Foreign.Storable.Offset.Internal.OffsetTH (offsetOf)
import Foreign.Storable.Offset.Internal.OffsetSelectable (OffsetSelectable (..))

class Offset a where
  offsetof :: (OffsetSelectable b, Num c) => a -> b -> c

-- https://downloads.haskell.org/~ghc/7.0.2/docs/html/users_guide/template-haskell.html
-- > A name can be quoted with either one or two prefix single quotes:
-- >   'f has type Name, and names the function f. Similarly 'C has type Name and names the data constructor C. In general 'thing interprets thing in an expression context.
-- >   ''T has type Name, and names the type constructor T. That is, ''thing interprets thing in a type context.
-- example:
--   makeOffset ''Foo

makeOffset :: Name -> Q [Dec]
makeOffset name = do
  let func = offsetOf name :: Q Exp
  let i = conT name
  [d|instance Offset $(i) where
      offsetof _ = $(func) . select
    |]
