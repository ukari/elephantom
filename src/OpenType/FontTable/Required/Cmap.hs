{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module OpenType.FontTable.Required.Cmap
  (
  ) where

import OpenType.DataTypes

-- | cmap — Character to Glyph Index Mapping Table
data Cmap = Cmap
  { version :: UINT16 -- ^ uint16 version Table version number (0).
  , numTables :: UINT16 -- ^ uint16 numTables Number of encoding tables that follow.
  , encodingRecords :: [ EncodingRecord ] -- ^ EncodingRecord encodingRecords[numTables]
  } deriving (Show)

data EncodingRecord = EncodingRecord
  { platformID :: UINT16 -- ^ uint16 platformID Platform ID.
  , encodingID :: UINT16 -- ^ uint16 encodingID Platform-specific encoding ID.
  , subtableOffset :: OFFSET32 -- ^ Offset32 subtableOffset Byte offset from beginning of table to the subtable for this encoding.
  } deriving (Show)

-- Platform IDs
{-# DEPRECATED ISO "deprecated in opentype spec" #-}
pattern Unicode = 0
pattern Macintosh = 1
pattern ISO = 2
pattern Windows = 3
pattern Custom = 4

-- Encoding IDs
-- Unicode platform (platform ID = 0)
{-# DEPRECATED Unicode_1_0, Unicode_1_1, ISO_IEC_10646 "deprecated in opentype spec" #-}
pattern Unicode_1_0 x = EncodingRecord Unicode 0 x -- ^ Unicode 1.0 semantics—deprecated
pattern Unicode_1_1 x = EncodingRecord Unicode 1 x -- ^ Unicode 1.1 semantics—deprecated
pattern ISO_IEC_10646 x = EncodingRecord Unicode 2 x -- ^ ISO/IEC 10646 semantics—deprecated
pattern Unicode_2_0_BMPOnly x = EncodingRecord Unicode 3 x -- ^ Unicode 2.0 and onwards semantics, Unicode BMP only
pattern Unicode_2_0_Full x = EncodingRecord Unicode 4 x -- ^ Unicode 2.0 and onwards semantics, Unicode full repertoire
pattern UnicodeVariationSequences_Format_14 x = EncodingRecord Unicode 5 x -- ^ Unicode Variation Sequences—for use with subtable format 14
pattern UnicodeFullRepertoire_Format_13 x = EncodingRecord Unicode 6 x -- ^ Unicode full repertoire—for use with subtable format 13

-- Macintosh platform (platform ID = 1)
-- Older Macintosh versions required fonts to have a 'cmap' subtable for platform ID 1. For current Apple platforms, use of platform ID 1 is discouraged. See the 'name' table chapter for details regarding encoding IDs defined for the Macintosh platform.
-- Macintosh encoding IDs (script manager codes)
pattern Roman x = EncodingRecord Macintosh 0 x
pattern Japanese x = EncodingRecord Macintosh 1 x
pattern ChineseTraditional x = EncodingRecord Macintosh 2 x
pattern Korean x = EncodingRecord Macintosh 3 x
pattern Arabic x = EncodingRecord Macintosh 4 x
pattern Hebrew x = EncodingRecord Macintosh 5 x
pattern Greek x = EncodingRecord Macintosh 6 x
pattern Russian x = EncodingRecord Macintosh 7 x
pattern RSymbol x = EncodingRecord Macintosh 8 x
pattern Devanagari x = EncodingRecord Macintosh 9 x
pattern Gurmukhi x = EncodingRecord Macintosh 10 x
pattern Gujarati x = EncodingRecord Macintosh 11 x
pattern Oriya x = EncodingRecord Macintosh 12 x
pattern Bengali x = EncodingRecord Macintosh 13 x
pattern Tamil x = EncodingRecord Macintosh 14 x
pattern Telugu x = EncodingRecord Macintosh 15 x
pattern Kannada x = EncodingRecord Macintosh 16 x
pattern Malayalam x = EncodingRecord Macintosh 17 x
pattern Sinhalese x = EncodingRecord Macintosh 18 x
pattern Burmese x = EncodingRecord Macintosh 19 x
pattern Khmer x = EncodingRecord Macintosh 20 x
pattern Thai x = EncodingRecord Macintosh 21 x
pattern Laotian x = EncodingRecord Macintosh 22 x
pattern Georgian x = EncodingRecord Macintosh 23 x
pattern Armenian x = EncodingRecord Macintosh 24 x
pattern ChineseSimplified x = EncodingRecord Macintosh 25 x
pattern Tibetan x = EncodingRecord Macintosh 26 x
pattern Mongolian x = EncodingRecord Macintosh 27 x
pattern Geez x = EncodingRecord Macintosh 28 x
pattern Slavic x = EncodingRecord Macintosh 29 x
pattern Vietnamese x = EncodingRecord Macintosh 30 x
pattern Sindi x = EncodingRecord Macintosh 31 x
pattern Uninterpreted x = EncodingRecord Macintosh 32 x

-- ISO platform (platform ID = 2)
-- Note: use of this platform ID is deprecated.
pattern ASCII_7_Bit x = EncodingRecord ISO 0 x
pattern ISO_10646 x = EncodingRecord ISO 1 x
pattern ISO_8859_1 x = EncodingRecord ISO 2 x

-- Windows platform (platform ID = 3)
pattern Symbol x = EncodingRecord Windows 0 x
pattern UnicodeBMP x = EncodingRecord Windows 1 x
pattern ShiftJIS x = EncodingRecord Windows 2 x
pattern PRC x = EncodingRecord Windows 3 x
pattern Big5 x = EncodingRecord Windows 4 x
pattern Wansung x = EncodingRecord Windows 5 x
pattern Johab x = EncodingRecord Windows 6 x
pattern Reversed x <- (isReversed -> (True, x))
pattern UnicodeFullRepertoire x = EncodingRecord Windows 10 x

isReversed :: EncodingRecord -> (Bool, OFFSET32)
isReversed (EncodingRecord Windows 7 y) = (True, y)
isReversed (EncodingRecord Windows 8 y) = (True, y)
isReversed (EncodingRecord Windows 9 y) = (True, y)
isReversed (EncodingRecord _ _ y) = (False, y)

-- Custom platform (platform ID = 4) and OTF Windows NT compatibility mapping
pattern OTFWindowsNT x <- (isOTFWindowsNT -> (True, x)) -- ^ 0-255 OTF Windows NT compatibility mapping

isOTFWindowsNT (EncodingRecord Custom x y) | x >= 0 && x <= 255 = (True, y)
isOTFWindowsNT (EncodingRecord _ _ y) = (False, y)

-- Use of the language field in 'cmap' subtables
-- The language field must be set to zero for all 'cmap' subtables whose platform IDs are other than Macintosh (platform ID 1). For 'cmap' subtables whose platform IDs are Macintosh, set this field to the Macintosh language ID of the 'cmap' subtable plus one, or to zero if the 'cmap' subtable is not language-specific. For example, a Mac OS Turkish 'cmap' subtable must set this field to 18, since the Macintosh language ID for Turkish is 17. A Mac OS Roman 'cmap' subtable must set this field to 0, since Mac OS Roman is not a language-specific encoding.

-- | 'cmap' Subtable Formats
data SubtableFormat
  = SubtF0 SubtableFormat_0
  | SubtF2 SubtableFormat_2
  | SubtF4 SubtableFormat_4
  | SubtF6 SubtableFormat_6
  | SubtF8 SubtableFormat_8
  | SubtF10 SubtableFormat_10
  | SubtF12 SubtableFormat_12
  | SubtF13 SubtableFormat_13
  | SubtF14 SubtableFormat_14
  deriving (Show)

-- | 'cmap' Subtable Format 0
data SubtableFormat_0 = SubtableFormat_0 
  { format :: UINT16 -- ^ uint16 format Format number is set to 0.
  , length :: UINT16 -- ^ uint16 length This is the length in bytes of the subtable.
  , language :: UINT16 -- ^ uint16 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , glyphIdArray :: [ UINT8 ] -- ^ uint8 glyphIdArray[256] An array that maps character codes to glyph index values.
  } deriving (Show)

-- | 'cmap' Subtable Format 2
data SubtableFormat_2 = SubtableFormat_2
  { format :: UINT16 -- ^ uint16 format Format number is set to 2.
  , length :: UINT16 -- ^ uint16 This is the length in bytes of the subtable.
  , language :: UINT16 -- ^ uint16 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , subHeaderKeys :: [ UINT16 ] -- ^ uint16 subHeaderKeys[256] Array that maps high bytes to subHeaders: value is subHeader index × 8.
  , subHeaders :: [ SubHeaderRecord ] -- ^ SubHeader subHeaders[ ] Variable-length array of SubHeader records.
  , glyphIdArray :: [ UINT16 ] -- ^ uint16 glyphIdArray[ ] Variable-length array containing subarrays used for mapping the low byte of 2-byte characters.
  } deriving (Show)

-- | SubHeader Record
data SubHeaderRecord = SubHeaderRecord
  { firstCode :: UINT16 -- ^ uint16 firstCode First valid low byte for this SubHeader.
  , entryCount :: UINT16 -- ^ uint16 entryCount Number of valid low bytes for this SubHeader.
  , idDelta :: INT16 -- ^ int16 idDelta Finally, if the value obtained from the subarray is not 0 (which indicates the missing glyph), you should add idDelta to it in order to get the glyphIndex. The value idDelta permits the same subarray to be used for several different subheaders. The idDelta arithmetic is modulo 65536.
  , idRangeOffset :: UINT16 -- ^ uint16 idRangeOffset The firstCode and entryCount values specify a subrange that begins at firstCode and has a length equal to the value of entryCount. This subrange stays within the 0-255 range of the byte being mapped. Bytes outside of this subrange are mapped to glyph index 0 (missing glyph).The offset of the byte within this subrange is then used as index into a corresponding subarray of glyphIdArray. This subarray is also of length entryCount. The value of the idRangeOffset is the number of bytes past the actual location of the idRangeOffset word where the glyphIdArray element corresponding to firstCode appears.
  } deriving (Show)

-- | 'cmap' Subtable Format 4
data SubtableFormat_4 = SubtableFormat_4
  { format :: UINT16 -- ^ uint16 format Format number is set to 4.
  , length :: UINT16 -- ^ uint16 length This is the length in bytes of the subtable.
  , language :: UINT16 -- ^ uint16 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , segCountX2 :: UINT16 -- ^ uint16 segCountX2 2 × segCount.
  , searchRange :: UINT16 -- ^ uint16 searchRange Maximum power of 2 less than or equal to segCount, times 2 ((2**floor(log2(segCount))) * 2, where “**” is an exponentiation operator)
  , entrySelector :: UINT16 -- ^ uint16 entrySelector Log2 of the maximum power of 2 less than or equal to numTables (log2(searchRange/2), which is equal to floor(log2(numTables)))
  , rangeShift :: UINT16 -- ^ uint16 rangeShift segCount times 2, minus searchRange ((segCount * 2) - searchRange)
  , endCode :: [ UINT16 ] -- ^ uint16 endCode[segCount] End characterCode for each segment, last=0xFFFF.
  , reservedPad :: UINT16 -- ^ uint16 reservedPad Set to 0.
  , startCode :: [ UINT16 ] -- ^ uint16 startCode[segCount] Start character code for each segment.
  , idDelta :: [ INT16 ] -- ^ int16 idDelta[segCount] Delta for all character codes in segment.
  , idRangeOffsets :: [ UINT16 ] -- ^ uint16 idRangeOffsets[segCount] Offsets into glyphIdArray or 0
  , glyphIdArray :: [ UINT16 ] -- ^ uint16 glyphIdArray[ ] Glyph index array (arbitrary length)
  } deriving (Show)

-- | 'cmap' Subtable Format 6
data SubtableFormat_6 = SubtableFormat_6
  { format :: UINT16 -- ^ uint16 format Format number is set to 6.
  , length :: UINT16 -- ^ uint16 length This is the length in bytes of the subtable.
  , language :: UINT16 -- ^ uint16 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , firstCode :: UINT16 -- ^ uint16 firstCode First character code of subrange.
  , entryCount :: UINT16 -- ^ uint16 entryCount Number of character codes in subrange.
  , glyphIdArray :: [ UINT16 ] -- ^ uint16 glyphIdArray[entryCount] Array of glyph index values for character codes in the range.
  } deriving (Show)

-- | 'cmap' Subtable Format 8
data SubtableFormat_8 = SubtableFormat_8
  { format :: UINT16 -- ^ uint16 format Subtable format; set to 8.
  , reserved :: UINT16 -- ^ uint16 reserved Reserved; set to 0
  , length :: UINT32 -- ^ uint32 length Byte length of this subtable (including the header)
  , language :: UINT32 -- ^ uint32 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , is32 :: [ UINT8 ] -- ^ uint8 is32[8192] Tightly packed array of bits (8K bytes total) indicating whether the particular 16-bit (index) value is the start of a 32-bit character code
  , numGroups :: UINT32 -- ^ uint32 numGroups Number of groupings which follow
  , groups :: [ SequentialMapGroupRecord ] -- ^ SequentialMapGroup groups[numGroups] Array of SequentialMapGroup records.
  } deriving (Show)

data SequentialMapGroupRecord = SequentialMapGroupRecord
  { startCharCode :: UINT32 -- ^ uint32 startCharCode First character code in this group; note that if this group is for one or more 16-bit character codes (which is determined from the is32 array), this 32-bit value will have the high 16-bits set to zero
  , endCharCode :: UINT32 -- ^ uint32 endCharCode Last character code in this group; same condition as listed above for the startCharCode
  , startGlyphID :: UINT32 -- ^ uint32 startGlyphID Glyph index corresponding to the starting character code
  } deriving (Show)

-- | 'cmap' Subtable Format 10
data SubtableFormat_10 = SubtableFormat_10
  { format :: UINT16 -- ^ uint16 format Subtable format; set to 10.
  , reserved :: UINT16 -- ^ uint16 reserved Reserved; set to 0
  , length :: UINT32 -- ^ uint32 length Byte length of this subtable (including the header)
  , language :: UINT32 -- ^ uint32 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , startCharCode :: UINT32 -- ^ uint32 startCharCode First character code covered
  , numChars :: UINT32 -- ^ uint32 numChars Number of character codes covered
  , glyphIdArray :: [ UINT16 ] -- ^  uint16 glyphIdArray Array of glyph indices for the character codes covered
  } deriving (Show)

-- | 'cmap' Subtable Format 12
data SubtableFormat_12 = SubtableFormat_12
  { format :: UINT16 -- ^ uint16 format Subtable format; set to 12.
  , reserved :: UINT16 -- ^ uint16 reserved Reserved; set to 0
  , length :: UINT32 -- ^ uint32 length Byte length of this subtable (including the header)
  , language :: UINT32 -- ^ uint32 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , numGroups :: UINT32 -- ^ uint32 numGroups Number of groupings which follow
  , groups :: [ SequentialMapGroupRecord ] -- ^ SequentialMapGroup groups[numGroups] Array of SequentialMapGroup records.
  } deriving (Show)

-- | 'cmap' Subtable Format 13
data SubtableFormat_13 = SubtableFormat_13
  { format :: UINT16 -- ^ uint16 format Subtable format; set to 13.
  , reserved :: UINT16 -- ^ uint16 reserved Reserved; set to 0
  , length :: UINT32 -- ^ uint32 length Byte length of this subtable (including the header)
  , language :: UINT32 -- ^ uint32 language For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.
  , numGroups :: UINT32 -- ^ uint32 numGroups Number of groupings which follow
  , groups :: [ ConstantMapGroupRecord ] -- ^ ConstantMapGroup groups[numGroups] Array of ConstantMapGroup records.
  } deriving (Show)

-- | ConstantMapGroup Record
-- The constant map group record has the same structure as the sequential map group record, with start and end character codes and a mapped glyph ID. However, the same glyph ID applies to all characters in the specified range rather than sequential glyph IDs.
data ConstantMapGroupRecord = ConstantMapGroupRecord
  { startCharCode :: UINT32 -- ^ uint32 startCharCode First character code in this group
  , endCharCode :: UINT32 -- ^ uint32 endCharCode Last character code in this group
  , glyphID :: UINT32 -- ^ uint32 glyphID Glyph index to be used for all the characters in the group’s range.
  } deriving (Show)

-- | 'cmap' Subtable Format 14
data SubtableFormat_14 = SubtableFormat_14
  { format :: UINT16 -- ^ uint16 format Subtable format. Set to 14.
  , length :: UINT32 -- ^ uint32 length Byte length of this subtable (including this header)
  , numVarSelectorRecords :: UINT32 -- ^ uint32 numVarSelectorRecords Number of variation Selector Records
  , varSelector :: [ VariationSelectorRecord ] -- ^ VariationSelector varSelector[numVarSelectorRecords] Array of VariationSelector records.
  } deriving (Show)

-- | VariationSelector Record
data VariationSelectorRecord = VariationSelectorRecord
  { varSelector :: UINT24 -- ^ uint24 varSelector Variation selector
  , defaultUVSOffset :: OFFSET32 -- ^ Offset32 defaultUVSOffset Offset from the start of the format 14 subtable to Default UVS Table. May be 0.
  , nonDefaultUVSOffset :: OFFSET32 -- ^ Offset32 nonDefaultUVSOffset Offset from the start of the format 14 subtable to Non-Default UVS Table. May be 0.
  } deriving (Show)

-- | DefaultUVS Table
data DefaultUVSTable = DefaultUVSTable
  { numUnicodeValueRanges :: UINT32 -- ^ uint32 numUnicodeValueRanges Number of Unicode character ranges.
  , ranges :: [ UnicodeRangeRecord ] -- ^ UnicodeRange ranges[numUnicodeValueRanges] Array of UnicodeRange records.
  } deriving (Show)

-- | UnicodeRange Record
data UnicodeRangeRecord = UnicodeRangeRecord
  { startUnicodeValue :: UINT24 -- ^ uint24 startUnicodeValue First value in this range
  , additionalCount :: UINT8 -- ^ uint8 additionalCount Number of additional values in this range
  } deriving (Show)

-- | NonDefaultUVS Table
data NonDefaultUVSTable = NonDefaultUVSTable
  { numUVSMappings :: UINT32 -- ^ uint32 numUVSMappings Number of UVS Mappings that follow
  , uvsMappings :: [ UVSMappingRecord ] -- ^ UVSMapping uvsMappings[numUVSMappings] Array of UVSMapping records.
  } deriving (Show)

-- | UVSMapping Record
data UVSMappingRecord = UVSMappingRecord
  { unicodeValue :: UINT24 -- ^ uint24 unicodeValue Base Unicode value of the UVS
  , glyphID :: UINT16 -- ^ uint16 glyphID Glyph ID of the UVS
  } deriving (Show)
