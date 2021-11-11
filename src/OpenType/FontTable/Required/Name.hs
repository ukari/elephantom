{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

module OpenType.FontTable.Required.Name
  (
  ) where

import OpenType.DataTypes

-- | name — Naming Table
data NamingTableHeader
  = NameV0 NamingTable_0
  | NameV1 NamingTable_1
  deriving (Show)

-- | Naming table version 0
data NamingTable_0 = NamingTable_0
  { version :: UINT16 -- ^ uint16 version Table version number (=0).
  , count :: UINT16 -- ^ uint16 count Number of name records.
  , storageOffset :: OFFSET16 -- ^ Offset16 storageOffset Offset to start of string storage (from start of table).
  , nameRecord :: [ NameRecord ] -- ^ NameRecord nameRecord[count] The name records where count is the number of records.
  } deriving (Show)

-- | Naming table version 1
data NamingTable_1 = NamingTable_1
  { version :: UINT16 -- ^ uint16 version Table version number (=1).
  , count :: UINT16 -- ^ uint16 count Number of name records.
  , storageOffset :: OFFSET16 -- ^ Offset16 storageOffset Offset to start of string storage (from start of table).
  , nameRecord :: [ NameRecord ] -- ^ NameRecord nameRecord[count] The name records where count is the number of records.
  , langTagCount :: UINT16 -- ^ uint16 langTagCount Number of language-tag records.
  , langTagRecord :: [ LangTagRecord ] -- ^ LangTagRecord langTagRecord[langTagCount] The language-tag records where langTagCount is the number of records.
  } deriving (Show)

-- | LangTagRecord
data LangTagRecord = LangTagRecord
  { length :: UINT16 -- ^ uint16 length Language-tag string length (in bytes)
  , langTagOffset :: OFFSET16 -- ^ Offset16 langTagOffset Language-tag string offset from start of storage area (in bytes).
  } deriving (Show)

-- | Name Record
data NameRecord = NameRecord
  { platformID :: UINT16 -- ^ uint16 platformID Platform ID.
  , encodingID :: UINT16 -- ^ uint16 encodingID Platform-specific encoding ID.
  , languageID :: UINT16 -- ^ uint16 languageID Language ID.
  , nameID :: UINT16 -- ^ uint16 nameID Name ID.
  , length :: UINT16 -- ^ uint16 length String length (in bytes).
  , stringOffset :: OFFSET16 -- ^ Offset16 stringOffset String offset from start of storage area (in bytes).
  } deriving (Show)

-- Name IDs
-- The following name IDs are pre-defined, and they apply to all platforms unless indicated otherwise. Name IDs 26 to 255, inclusive, are reserved for future standard names. Name IDs 256 to 32767, inclusive, are reserved for font-specific names such as those referenced by a font’s layout features.

-- | 0 Copyright notice.
pattern CopyrightNotice = 0

-- | 1 Font Family name.
-- The Font Family name is used in combination with Font Subfamily name (name ID 2), and should be shared among at most four fonts that differ only in weight or style (as described below).
-- This four-way distinction should also be reflected in the OS/2.fsSelection field, using bits 0 and 5.
-- While some platforms or applications do not have this constraint, many existing applications that use this pair of names assume that a Font Family name is shared by at most four fonts that form a font style-linking group: regular, italic (or oblique), bold, and bold italic (or bold oblique). To be compatible with the broadest range of platforms and applications, it is strongly recommended that fonts limit use of Font Family name in this manner.
-- For extended typographic families that includes fonts other than the four basic styles (regular, italic, bold, bold italic), it is strongly recommended that name IDs 16 and 17 be used in fonts to create an extended, typographic grouping. (See examples provided below.)
-- It is also strongly recommended that applications support extended typographic-family groupings using name IDs 16 and 17. Note, in particular, that variable fonts can include a large number of named instances, each of which will use a shared typographic family name (name ID 16) and will have a typographic subfamily name (equivalent to name ID 17). Applications that assume a four-style family grouping based on name IDs 1 and 2 are likely to provide a poor user experience with variable fonts.
-- For fonts within an extended typographic family that fall outside the basic four-way distinction, the distinguishing attributes should be reflected in the Font Family name so that those fonts appear as a separate font family. For example, the Font Family name for the Arial Narrow font is “Arial Narrow”; the Font Family name for the Arial Black font is “Arial Black”. Note that, in such cases, name ID 16 should also be included with a shared name that reflects the full, typographic family.
pattern FontFamilyName = 1

-- | 2 Font Subfamily name.
-- The Font Subfamily name is used in combination with Font Family name (name ID 1), and distinguishes the fonts in a group with the same Font Family name. This should be used for style and weight variants only (as described below).
-- This four-way distinction should also be reflected in the OS/2.fsSelection field, using bits 0 and 5. A font with no distinctive weight or style (e.g. medium weight, not italic, and OS/2.fsSelection bit 6 set) should use the string “Regular” as the Font Subfamily name (for English language).
-- While some platforms or applications do not have this constraint, many existing applications that use this pair of names assume that a Font Family name is shared by at most four fonts that form a font style-linking group, and that Font Subfamily names would reflect one of the four basic styles, regular, italic (or oblique), bold, and bold italic (or bold oblique). To be compatible with the broadest range of platforms and applications, it is strongly recommended that fonts should limit use of Font Family in this manner.
-- For extended typographic families that includes fonts other than the four basic styles (regular, italic, bold, bold italic), it is strongly recommended that name IDs 16 and 17 be used in fonts to create an extended, typographic grouping.
-- Within an extended typographic family that includes fonts beyond regular, bold, italic, or bold italic, distinctions are made in the Font Family name, so that fonts appear to be in separate families. In some cases, this may lead to specifying a Subfamily name of “Regular” for a font that might not otherwise be considered a regular font. For example, the Arial Black font has a Font Family name of “Arial Black” and a Subfamily name of “Regular”. Note that, in such cases, name IDs 16 and 17 should also be included, using a shared value for name ID 16 that reflects the full typographic family, and values for name ID 17 that appropriately reflect the actual design variant of each font.
pattern FontSubfamilyName = 2

-- | 3 Unique font identifier
pattern UniqueFontIdentifier = 3

-- | 4 Full font name that reflects all family and relevant subfamily descriptors.
-- The full font name is generally a combination of name IDs 1 and 2, or of name IDs 16 and 17, or a similar human-readable variant.
-- For fonts in extended typographic families (that is, families that include more than regular, italic, bold, and bold italic variants), values for name IDs 1 and 2 are normally chosen to provide compatibility with certain applications that assume a family has at most four style-linked fonts. In that case, some fonts may end up with a Subfamily name (name ID 2) of “Regular” even though the font would not be considered, typographically, a regular font. For such non-regular fonts in which name ID 2 is specified as “Regular”, the “Regular” descriptor would generally be omitted from name ID 4. For example, the Arial Black font has a Font Family name (name ID 1) of “Arial Black” and a Subfamily name (name ID 2) of “Regular”, but has a full font name (name ID 4) of “Arial Black”. Note that name IDs 16 and 17 should also be included in these fonts, and that name ID 4 would typically be a combination of name IDs 16 and 17, without needing any additional qualifications regarding “Regular”.
pattern FullFontName = 4

-- | 5 Version string.
-- Should begin with the syntax “Version <number>.<number>” (upper case, lower case, or mixed, with a space between “Version” and the number).
-- The string must contain a version number of the following form: one or more digits (0-9) of value less than 65,535, followed by a period, followed by one or more digits of value less than 65,535. Any character other than a digit will terminate the minor number. A character such as “;” is helpful to separate different pieces of version information.
-- The first such match in the string can be used by installation software to compare font versions. Note that some installers may require the string to start with “Version ”, followed by a version number as above.
pattern VersionString = 5

-- | 6 PostScript name for the font;
-- Name ID 6 specifies a string which is used to invoke a PostScript language font that corresponds to this OpenType font. When translated to ASCII, the name string must be no longer than 63 characters and restricted to the printable ASCII subset, codes 33 to 126, except for the 10 characters '[', ']', '(', ')', '{', '}', '<', '>', '/', '%'.
-- In a CFF OpenType font, there is no requirement that this name be the same as the font name in the CFF’s Name INDEX. Thus, the same CFF may be shared among multiple font components in a Font Collection. See the 'name' table section of “Recommendations for OpenType fonts” for additional information.
pattern PostScriptName = 6

-- | 7 Trademark;
-- this is used to save any trademark notice/information for this font. Such information should be based on legal advice. This is distinctly separate from the copyright.
pattern Trademark = 7

-- | 8 Manufacturer Name.
pattern ManufacturerName = 8

-- | 9 Designer;
-- name of the designer of the typeface.
pattern Designer = 9

-- | 10 Description;
-- description of the typeface. Can contain revision information, usage recommendations, history, features, etc.
pattern Description = 10

-- | 11 URL Vendor;
-- URL of font vendor (with protocol, e.g., http://, ftp://). If a unique serial number is embedded in the URL, it can be used to register the font.
pattern URLVendor = 11

-- | 12 URL Designer;
-- URL of typeface designer (with protocol, e.g., http://, ftp://).
pattern URLDesigner = 12

-- | 13 License Description;
-- description of how the font may be legally used, or different example scenarios for licensed use. This field should be written in plain language, not legalese.
pattern LicenseDescription = 13

-- | 14 License Info URL;
-- URL where additional licensing information can be found.
pattern LicenseInfoURL = 14

-- | 15 Reserved.
pattern Reserved = 15

-- | 16 Typographic Family name:
-- The typographic family grouping doesn’t impose any constraints on the number of faces within it, in contrast with the 4-style family grouping (ID 1), which is present both for historical reasons and to express style linking groups. If name ID 16 is absent, then name ID 1 is considered to be the typographic family name. (In earlier versions of the specification, name ID 16 was known as “Preferred Family”.)
pattern TypographicFamilyName = 16

-- | 17 Typographic Subfamily name:
-- This allows font designers to specify a subfamily name within the typographic family grouping. This string must be unique within a particular typographic family. If it is absent, then name ID 2 is considered to be the typographic subfamily name. (In earlier versions of the specification, name ID 17 was known as “Preferred Subfamily”.)
pattern TypographicSubfamilyName = 17

-- | 18 Compatible Full (Macintosh only);
-- On the Macintosh, the menu name is constructed using the FOND resource. This usually matches the Full Name. If you want the name of the font to appear differently than the Full Name, you can insert the Compatible Full Name in ID 18.
pattern CompatibleFullMacintoshOnly = 18

-- | 19 Sample Text;
-- This can be the font name, or any other text that the designer thinks is the best sample to display the font in.
pattern SampleText = 19

-- | 20 PostScriptCID findfont name;
-- Its presence in a font means that the nameID 6 holds a PostScript font name that is meant to be used with the “composefont” invocation in order to invoke the font in a PostScript interpreter. See the definition of name ID 6.
-- The value held in the name ID 20 string is interpreted as a PostScript font name that is meant to be used with the “findfont” invocation, in order to invoke the font in a PostScript interpreter.
-- When translated to ASCII, this name string must be restricted to the printable ASCII subset, codes 33 through 126, except for the 10 characters: '[', ']', '(', ')', '{', '}', '<', '>', '/', '%'.
-- See “Recommendations for OTF fonts” for additional information
pattern PostScriptCIDFindFontName = 20

-- | 21 WWS Family Name.
-- Used to provide a WWS-conformant family name in case the entries for IDs 16 and 17 do not conform to the WWS model. (That is, in case the entry for ID 17 includes qualifiers for some attribute other than weight, width or slope.) If bit 8 of the fsSelection field is set, a WWS Family Name entry should not be needed and should not be included. Conversely, if an entry for this ID is included, bit 8 should not be set. (See OS/2.fsSelection field for details.) Examples of name ID 21: “Minion Pro Caption” and “Minion Pro Display”. (Name ID 16 would be “Minion Pro” for these examples.)
pattern WWSFamilyName = 21

-- | 22 WWS Subfamily Name.
-- Used in conjunction with ID 21, this ID provides a WWS-conformant subfamily name (reflecting only weight, width and slope attributes) in case the entries for IDs 16 and 17 do not conform to the WWS model. As in the case of ID 21, use of this ID should correlate inversely with bit 8 of the fsSelection field being set. Examples of name ID 22: “Semibold Italic”, “Bold Condensed”. (Name ID 17 could be “Semibold Italic Caption”, or “Bold Condensed Display”, for example.)
pattern WWSSubfamilyName = 22

-- | 23 Light Background Palette.
-- This ID, if used in the CPAL table’s Palette Labels Array, specifies that the corresponding color palette in the CPAL table is appropriate to use with the font when displaying it on a light background such as white. Strings for this ID are for use as user interface strings associated with this palette.
pattern LightBackgroundPalette = 23

-- | 24 Dark Background Palette.
-- This ID, if used in the CPAL table’s Palette Labels Array, specifies that the corresponding color palette in the CPAL table is appropriate to use with the font when displaying it on a dark background such as black. Strings for this ID are for use as user interface strings associated with this palette
pattern DarkBackgroundPalette = 24

-- | 25 Variations PostScript Name Prefix.
-- If present in a variable font, it may be used as the family prefix in the PostScript Name Generation for Variation Fonts algorithm. The character set is restricted to ASCII-range uppercase Latin letters, lowercase Latin letters, and digits. All name strings for name ID 25 within a font, when converted to ASCII, must be identical. See Adobe Technical Note #5902: “PostScript Name Generation for Variation Fonts” for reasons to include name ID 25 in a font, and for examples. For general information on OpenType Font Variations, see the chapter, OpenType Font Variations Overview.
pattern VariationsPostScriptNamePrefix = 25

