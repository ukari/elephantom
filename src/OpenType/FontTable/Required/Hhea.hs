module OpenType.FontTable.Required.Hhea
  (
  ) where

import OpenType.DataTypes

-- | hhea — Horizontal Header Table
data Hhea = Hhea
  { majorVersion :: UINT16 -- ^ uint16 majorVersion Major version number of the horizontal header table — set to 1.
  , minorVersion :: UINT16 -- ^ uint16 minorVersion Minor version number of the horizontal header table — set to 0.
  , ascender :: FWORD -- ^ FWORD ascender Typographic ascent—see note below.
  , descender :: FWORD -- ^ FWORD descender Typographic descent—see note below.
  , lineGap :: FWORD -- ^ FWORD lineGap Typographic line gap. Negative LineGap values are treated as zero in some legacy platform implementations.
  , advanceWidthMax :: UFWORD -- ^ UFWORD advanceWidthMax Maximum advance width value in 'hmtx' table.
  , minLeftSideBearing :: FWORD -- ^ FWORD minLeftSideBearing Minimum left sidebearing value in 'hmtx' table for glyphs with contours (empty glyphs should be ignored).
  , minRightSideBearing :: FWORD -- ^ FWORD minRightSideBearing Minimum right sidebearing value; calculated as min(aw - (lsb + xMax - xMin)) for glyphs with contours (empty glyphs should be ignored).
  , xMaxExtent :: FWORD -- ^ FWORD xMaxExtent Max(lsb + (xMax - xMin)).
  , caretSlopeRise :: INT16 -- ^ int16 caretSlopeRise Used to calculate the slope of the cursor (rise/run); 1 for vertical.
  , caretSlopeRun :: INT16 -- ^ int16 caretSlopeRun 0 for vertical.
  , caretOffset :: INT16 -- ^ int16 caretOffset The amount by which a slanted highlight on a glyph needs to be shifted to produce the best appearance. Set to 0 for non-slanted fonts
  , reserved0 :: INT16 -- ^ int16 (reserved) set to 0
  , reserved1 :: INT16 -- ^ int16 (reserved) set to 0
  , reserved2 :: INT16 -- ^ int16 (reserved) set to 0
  , reserved3 :: INT16 -- ^ int16 (reserved) set to 0
  , metricDataFormat :: INT16 -- ^ int16 metricDataFormat 0 for current format.
  , numberOfHMetrics :: UINT16 -- ^ uint16 numberOfHMetrics Number of hMetric entries in 'hmtx' table
  } deriving (Show)
