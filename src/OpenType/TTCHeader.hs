{-# LANGUAGE DuplicateRecordFields #-}

module OpenType.TTCHeader
  (
  ) where

import OpenType.DataTypes

data TTCHeader
  = TTCH1 TTCHeader_1_0
  | TTCH2 TTCHeader_2_0
  deriving (Show)

data TTCHeader_1_0 = TTCHeader_1_0
  { ttcTag :: TAG -- ^ TAG ttcTag Font Collection ID string: 'ttcf' (used for fonts with CFF or CFF2 outlines as well as TrueType outlines)
  , majorVersion :: UINT16 -- ^ uint16 majorVersion Major version of the TTC Header, = 1.
  , minorVersion :: UINT16 -- ^ uint16 minorVersion Minor version of the TTC Header, = 0.
  , numFonts :: UINT32 -- ^ uint32 numFonts Number of fonts in TTC
  , tableDirectoryOffsets :: [ OFFSET32 ] -- ^ Offset32 tableDirectoryOffsets[numFonts] Array of offsets to the TableDirectory for each font from the beginning of the file
  } deriving (Show)

data TTCHeader_2_0 = TTCHeader_2_0
  { ttcTag :: TAG -- ^ TAG ttcTag Font Collection ID string: 'ttcf'
  , majorVersion :: UINT16 -- ^ uint16 majorVersion Major version of the TTC Header, = 2.
  , minorVersion :: UINT16 -- ^ uint16 minorVersion Minor version of the TTC Header, = 0.
  , numFonts :: UINT32 -- ^ uint32 numFonts Number of fonts in TTC
  , tableDirectoryOffsets :: [ OFFSET32 ] -- ^ Offset32 tableDirectoryOffsets[numFonts] Array of offsets to the TableDirectory for each font from the beginning of the file
  , dsigTag :: UINT32 -- ^ uint32 dsigTag Tag indicating that a DSIG table exists, 0x44534947 ('DSIG') (null if no signature)
  , dsigLength :: UINT32 -- ^ uint32 dsigLength The length (in bytes) of the DSIG table (null if no signature)
  , dsigOffset :: UINT32 -- ^ uint32 dsigOffset The offset (in bytes) of the DSIG table from the beginning of the TTC file (null if no signature)
  } deriving (Show)
