module OpenType.DataTypes
  ( UINT8
  , INT8
  , UINT16
  , INT16
  , UINT24
  , UINT32
  , INT32
  , FIXED
  , FWORD
  , UFWORD
  , F2DOT14
  , LONGDATETIME
  , TAG
  , OFFSET16
  , OFFSET32
  , VERSION16DOT16
  ) where


import Data.Word.Word24 (Word24)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int8, Int16, Int32, Int64)

-- spec: https://docs.microsoft.com/zh-cn/typography/opentype/spec/otff

-- ordering: Big Endian

-- | uint8
-- 8-bit unsigned integer.
type UINT8 = Word8

-- | int8
-- 8-bit signed integer.
type INT8 = Int8

-- | uint16
-- 16-bit unsigned integer.
type UINT16 = Word16

-- | int16
-- 16-bit signed integer.
type INT16 = Int16

-- | uint24
-- 24-bit unsigned integer.
type UINT24 = Word24

-- | uint32
-- 32-bit unsigned integer.
type UINT32 = Word32

-- | int32
-- 32-bit signed integer.
type INT32 = Int32

-- | Fixed
-- 32-bit signed fixed-point number (16.16)
type FIXED = Int32

-- | FWORD
-- int16 that describes a quantity in font design units.
type FWORD = Int16

-- | UFWORD
-- uint16 that describes a quantity in font design units.
type UFWORD = Word16

-- | F2DOT14
-- 16-bit signed fixed number with the low 14 bits of fraction (2.14).
type F2DOT14 = Int16

-- | LONGDATETIME
-- Date and time represented in number of seconds since 12:00 midnight, January 1, 1904, UTC. The value is represented as a signed 64-bit integer.
type LONGDATETIME = Int64

-- | Tag
-- Array of four uint8s (length = 32 bits) used to identify a table, design-variation axis, script, language system, feature, or baseline
type TAG = Word32

-- | Offset16
-- Short offset to a table, same as uint16, NULL offset = 0x0000
type OFFSET16 = Word16

-- | Offset32
-- Long offset to a table, same as uint32, NULL offset = 0x00000000
type OFFSET32 = Word32

-- | Version16Dot16
-- Packed 32-bit value with major and minor version numbers. (See Table Version Numbers.)
type VERSION16DOT16 = Word32
