module OpenType.TableDirectory
  (
  ) where

import OpenType.DataTypes

data TableDirectory = TableDirectory
  { sfntVersion :: UINT32 -- ^ uint32 sfntVersion 0x00010000 or 0x4F54544F ('OTTO') — see below.
  , numTables :: UINT16 -- ^ uint16 numTables Number of tables.
  , searchRange :: UINT16 -- ^ uint16 searchRange Maximum power of 2 less than or equal to numTables, times 16 ((2**floor(log2(numTables))) * 16, where “**” is an exponentiation operator).
  , entrySelector :: UINT16 -- ^ uint16 entrySelector Log2 of the maximum power of 2 less than or equal to numTables (log2(searchRange/16), which is equal to floor(log2(numTables))).
  , rangeShift :: UINT16 -- ^ uint16 rangeShift numTables times 16, minus searchRange ((numTables * 16) - searchRange).
  , tableRecords ::  [ TableRecord ] -- ^ tableRecord tableRecords[numTables] Table records array—one for each top-level table in the font
  } deriving (Show)

data TableRecord = TableRecord
  { tableTag :: TAG -- ^ Tag tableTag Table identifier.
  , checksum :: UINT32 -- ^ uint32 checksum Checksum for this table.
  , offset :: OFFSET32 -- ^ Offset32 offset Offset from beginning of font file.
  , length :: UINT32 -- ^ Length of this table.
  } deriving (Show)
