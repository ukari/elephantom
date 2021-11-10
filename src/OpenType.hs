
{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module OpenType
  (
  ) where


--import GHC.Exts (Word8#, Int8#, Word16#, Int16#, Word32#, Int32#, Int64#)

import Data.Attoparsec.ByteString.Lazy (Parser, Result (..), anyWord8, count, word8, parse, takeLazyByteString, eitherResult)
import qualified Data.Attoparsec.ByteString.Lazy as Attoparsec
import Data.Conduit.Attoparsec (conduitParserEither)

import Paths_elephantom (getDataFileName)

testfont :: IO ()
testfont = do
  fontfile <- getDataFileName "font/SourceCodePro-Regular.ttf"-- "font/NotoSansMonoCJKsc-Regular.otf"
  print fontfile
  

  print "test font"
