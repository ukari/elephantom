module OpenType.FontTable.Required.Hmtx
  (
  ) where

import OpenType.DataTypes

-- | hmtx — Horizontal Metrics Table
data Hmtx = Hmtx
  { hMetrics :: [ LongHorMetric ] -- ^ longHorMetric hMetrics[numberOfHMetrics] Paired advance width and left side bearing values for each glyph. Records are indexed by glyph ID.
  , leftSideBearings :: [ INT16 ] -- ^ int16 leftSideBearings[numGlyphs - numberOfHMetrics] Left side bearings for glyph IDs greater than or equal to numberOfHMetrics. 
  } deriving (Show)

data LongHorMetric = LongHorMetric
  { advanceWidth :: UINT16 -- ^ uint16 advanceWidth Advance width, in font design units.
  , lsb :: INT16 -- ^ int16 lsb Glyph left side bearing, in font design units.
  } deriving (Show)
