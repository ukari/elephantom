{-# LANGUAGE DuplicateRecordFields #-}

module OpenType.FontTable.Required.OS2
  (
  ) where

import OpenType.DataTypes

-- OS/2 — OS/2 and Windows Metrics Table

-- | OS/2 Table Formats
data OS2TableFormat
  = OS2TF5 OS2TableFormat_5
  deriving (Show)

-- | OS/2 Table Format Version 5
-- Version 5 was defined in OpenType 1.7. Version 5 has two additional fields beyond those in version 4. The format of version 5 is as follows:
data OS2TableFormat_5 = OS2TableFormat_5
  { version :: UINT16 -- ^ uint16 version 0x0005
  , xAvgCharWidth :: INT16 -- ^ int16 xAvgCharWidth
  , usWeightClass :: UINT16 -- ^ uint16 usWeightClass
  , usWidthClass :: UINT16 -- ^ uint16 usWidthClass
  , fsType :: UINT16 -- ^ uint16 fsType
  , ySubscriptXSize :: INT16 -- ^ int16 ySubscriptXSize
  , ySubscriptYSize :: INT16 -- ^ int16 ySubscriptYSize
  , ySubscriptXOffset :: INT16 -- ^ int16 ySubscriptXOffset
  , ySubscriptYOffset :: INT16 -- ^ int16 ySubscriptYOffset
  , ySuperscriptXSize :: INT16 -- ^ int16 ySuperscriptXSize
  , ySuperscriptYSize :: INT16 -- ^ int16 ySuperscriptYSize
  , ySuperscriptXOffset :: INT16 -- ^ int16 ySuperscriptXOffset
  , ySuperscriptYOffset :: INT16 -- ^ int16 ySuperscriptYOffset
  , yStrikeoutSize :: INT16 -- ^ int16 yStrikeoutSize
  , yStrikeoutPosition :: INT16 -- ^ int16 yStrikeoutPosition
  , sFamilyClass :: INT16 -- ^ int16 sFamilyClass
  , panose :: [ UINT8 ] -- ^ uint8 panose[10]
  , ulUnicodeRange1 :: UINT32 -- ^ uint32 ulUnicodeRange1 Bits 0–31
  , ulUnicodeRange2 :: UINT32 -- ^ uint32 ulUnicodeRange2 Bits 32–63
  , ulUnicodeRange3 :: UINT32 -- ^ uint32 ulUnicodeRange3 Bits 64–95
  , ulUnicodeRange4 :: UINT32 -- ^ uint32 ulUnicodeRange4 Bits 96–127
  , achVendID :: TAG -- ^ Tag achVendID
  , fsSelection :: UINT16 -- ^ uint16 fsSelection
  , usFirstCharIndex :: UINT16 -- ^ uint16 usFirstCharIndex
  , usLastCharIndex :: UINT16 -- ^ uint16 usLastCharIndex
  , sTypoAscender :: INT16 -- ^ int16 sTypoAscender
  , sTypoDescender :: INT16 -- ^ int16 sTypoDescender
  , sTypoLineGap :: INT16 -- ^ int16 sTypoLineGap
  , usWinAscent :: UINT16 -- ^ uint16 usWinAscent
  , usWinDescent :: UINT16 -- ^ uint16 usWinDescent
  , ulCodePageRange1 :: UINT32 -- ^ uint32 ulCodePageRange1 Bits 0–31
  , ulCodePageRange2 :: UINT32 -- ^ uint32 ulCodePageRange2 Bits 32–63
  , sxHeight :: INT16 -- ^ int16 sxHeight
  , sCapHeight :: INT16 -- ^ int16 sCapHeight
  , usDefaultChar :: UINT16 -- ^ uint16 usDefaultChar
  , usBreakChar :: UINT16 -- ^ uint16 usBreakChar
  , usMaxContext :: UINT16 -- ^ uint16 usMaxContext
  , usLowerOpticalPointSize :: UINT16 -- ^ uint16 usLowerOpticalPointSize
  , usUpperOpticalPointSize :: UINT16 -- ^ uint16 usUpperOpticalPointSize
  } deriving (Show)

-- | OS/2 Table Format Version 4
-- Version 4 was defined in OpenType 1.5. Version 4 has two fewer fields than version 5, and the same fields as in version 3. Although new fields were not added beyond those in version 3, the specification of certain fields was revised. The format of version 4 is as follows:
data OS2TableFormat_4 = OS2TableFormat_4
  { version :: UINT16 -- ^ uint16 version
  , xAvgCharWidth :: INT16 -- ^ int16 xAvgCharWidth
  , usWeightClass :: UINT16 -- ^ uint16 usWeightClass
  , usWidthClass :: UINT16 -- ^ uint16 usWidthClass
  , fsType :: UINT16 -- ^ uint16 fsType
  , ySubscriptXSize :: INT16 -- ^ int16 ySubscriptXSize
  , ySubscriptYSize :: INT16 -- ^ int16 ySubscriptYSize
  , ySubscriptXOffset :: INT16 -- ^ int16 ySubscriptXOffset
  , ySubscriptYOffset :: INT16 -- ^ int16 ySubscriptYOffset
  , ySuperscriptXSize :: INT16 -- ^ int16 ySuperscriptXSize
  , ySuperscriptYSize :: INT16 -- ^ int16 ySuperscriptYSize
  , ySuperscriptXOffset :: INT16 -- ^ int16 ySuperscriptXOffset
  , ySuperscriptYOffset :: INT16 -- ^ int16 ySuperscriptYOffset
  , yStrikeoutSize :: INT16 -- ^ int16 yStrikeoutSize
  , yStrikeoutPosition :: INT16 -- ^ int16 yStrikeoutPosition
  , sFamilyClass :: INT16 -- ^ int16 sFamilyClass
  , panose :: [ UINT8 ] -- ^ uint8 panose[10]
  , ulUnicodeRange1 :: UINT32 -- ^ uint32 ulUnicodeRange1
  , ulUnicodeRange2 :: UINT32 -- ^ uint32 ulUnicodeRange2
  , ulUnicodeRange3 :: UINT32 -- ^ uint32 ulUnicodeRange3
  , ulUnicodeRange4 :: UINT32 -- ^ uint32 ulUnicodeRange4
  , achVendID :: TAG -- ^ Tag achVendID
  , fsSelection :: UINT16 -- ^ uint16 fsSelection
  , usFirstCharIndex :: UINT16 -- ^ uint16 usFirstCharIndex
  , usLastCharIndex :: UINT16 -- ^ uint16 usLastCharIndex
  , sTypoAscender :: INT16 -- ^ int16 sTypoAscender
  , sTypoDescender :: INT16 -- ^ int16 sTypoDescender
  , sTypoLineGap :: INT16 -- ^ int16 sTypoLineGap
  , usWinAscent :: UINT16 -- ^ uint16 usWinAscent
  , usWinDescent :: UINT16 -- ^ uint16 usWinDescent
  , ulCodePageRange1 :: UINT32 -- ^ uint32 ulCodePageRange1
  , ulCodePageRange2 :: UINT32 -- ^ uint32 ulCodePageRange2
  , sxHeight :: INT16 -- ^ int16 sxHeight
  , sCapHeight :: INT16 -- ^ int16 sCapHeight
  , usDefaultChar :: UINT16 -- ^ uint16 usDefaultChar
  , usBreakChar :: UINT16 -- ^ uint16 usBreakChar
  , usMaxContext :: UINT16 -- ^ uint16 usMaxContext
  } deriving (Show)

-- TODO
