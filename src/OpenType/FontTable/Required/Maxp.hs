{-# LANGUAGE DuplicateRecordFields #-}

module OpenType.FontTable.Required
  (
  ) where

import OpenType.DataTypes

-- | maxp — Maximum Profile
data Maxp
  = MaxpCFF Maxp_0_5
  | MaxpTrueType Maxp_1_0
  deriving (Show)

data Maxp_0_5 = Maxp_0_5
  { version :: VERSION16DOT16 -- ^ Version16Dot16 version 0x00005000 for version 0.5
  , numGlyphs :: UINT16 -- ^ uint16 numGlyphs The number of glyphs in the font.
  } deriving (Show)

data Maxp_1_0 = Maxp_1_0
  { version :: VERSION16DOT16 -- ^ Version16Dot16 version 0x00010000 for version 1.0.
  , numGlyphs :: UINT16 -- ^ uint16 numGlyphs The number of glyphs in the font.
  , maxPoints :: UINT16 -- ^ uint16 maxPoints Maximum points in a non-composite glyph.
  , maxContours :: UINT16 -- ^ uint16 maxContours Maximum contours in a non-composite glyph.
  , maxCompositePoints :: UINT16 -- ^ uint16 maxCompositePoints Maximum points in a composite glyph.
  , maxCompositeContours :: UINT16 -- ^ uint16 maxCompositeContours Maximum contours in a composite glyph.
  , maxZones :: UINT16 -- ^ uint16 maxZones 1 if instructions do not use the twilight zone (Z0), or 2 if instructions do use Z0; should be set to 2 in most cases.
  , maxTwilightPoints :: UINT16 -- ^ uint16 maxTwilightPoints Maximum points used in Z0.
  , maxStorage :: UINT16 -- ^ uint16 maxStorage Number of Storage Area locations.
  , maxFunctionDefs :: UINT16 -- ^ uint16 maxFunctionDefs Number of FDEFs, equal to the highest function number + 1.
  , maxInstructionDefs :: UINT16 -- ^ uint16 maxInstructionDefs Number of IDEFs.
  , maxStackElements :: UINT16 -- ^ uint16 maxStackElements Maximum stack depth across Font Program ('fpgm' table), CVT Program ('prep' table) and all glyph instructions (in the 'glyf' table).
  , maxSizeOfInstructions :: UINT16 -- ^ uint16 maxSizeOfInstructions Maximum byte count for glyph instructions.
  , maxComponentElements :: UINT16 -- ^ uint16 maxComponentElements Maximum number of components referenced at “top level” for any composite glyph.
  , maxComponentDepth :: UINT16 -- ^ uint16 maxComponentDepth Maximum levels of recursion; 1 for simple components.
  } deriving (Show)
