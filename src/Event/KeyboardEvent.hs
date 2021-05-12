module Event.KeyboardEvent
  ( keyboardEvent
  ) where

import Event.Input
import Event.InputMotionEvent
import Event.Keyboard (KeyMotion (KeyMotion), Keycode (..))
import qualified SDL

keyboardEvent :: SDL.KeyboardEventData -> Input
keyboardEvent (SDL.KeyboardEventData _ input _ keysym) = Keyboard $ KeyMotion (toKeycode keysym) (mapInputMotion input)

toKeycode :: SDL.Keysym -> Keycode
toKeycode (SDL.Keysym _ keycode _)= mapKeycode keycode

mapKeycode :: SDL.Keycode -> Keycode
mapKeycode SDL.KeycodeReturn = KeyReturn
mapKeycode SDL.KeycodeEscape = KeyEscape
mapKeycode SDL.KeycodeBackspace = KeyBackspace
mapKeycode SDL.KeycodeTab = KeyTab
mapKeycode SDL.KeycodeSpace = KeySpace
mapKeycode SDL.KeycodeExclaim = KeyExclaim
mapKeycode SDL.KeycodeQuoteDbl = KeyQuoteDbl
mapKeycode SDL.KeycodeHash = KeyHash
mapKeycode SDL.KeycodePercent = KeyPercent
mapKeycode SDL.KeycodeDollar = KeyDollar
mapKeycode SDL.KeycodeAmpersand = KeyAmpersand
mapKeycode SDL.KeycodeQuote = KeyQuote
mapKeycode SDL.KeycodeLeftParen = KeyLeftParen
mapKeycode SDL.KeycodeRightParen = KeyRightParen
mapKeycode SDL.KeycodeAsterisk = KeyAsterisk
mapKeycode SDL.KeycodePlus = KeyPlus
mapKeycode SDL.KeycodeComma = KeyComma
mapKeycode SDL.KeycodeMinus = KeyMinus
mapKeycode SDL.KeycodePeriod = KeyPeriod
mapKeycode SDL.KeycodeSlash = KeySlash
mapKeycode SDL.Keycode0 = Key0
mapKeycode SDL.Keycode1 = Key1
mapKeycode SDL.Keycode2 = Key2
mapKeycode SDL.Keycode3 = Key3
mapKeycode SDL.Keycode4 = Key4
mapKeycode SDL.Keycode5 = Key5
mapKeycode SDL.Keycode6 = Key6
mapKeycode SDL.Keycode7 = Key7
mapKeycode SDL.Keycode8 = Key8
mapKeycode SDL.Keycode9 = Key9
mapKeycode SDL.KeycodeColon = KeyColon
mapKeycode SDL.KeycodeSemicolon = KeySemicolon
mapKeycode SDL.KeycodeLess = KeyLess
mapKeycode SDL.KeycodeEquals = KeyEquals
mapKeycode SDL.KeycodeGreater = KeyGreater
mapKeycode SDL.KeycodeQuestion = KeyQuestion
mapKeycode SDL.KeycodeAt = KeyAt
mapKeycode SDL.KeycodeLeftBracket = KeyLeftBracket
mapKeycode SDL.KeycodeBackslash = KeyBackslash
mapKeycode SDL.KeycodeRightBracket = KeyRightBracket
mapKeycode SDL.KeycodeCaret = KeyCaret
mapKeycode SDL.KeycodeUnderscore = KeyUnderscore
mapKeycode SDL.KeycodeBackquote = KeyBackquote
mapKeycode SDL.KeycodeA = KeyA
mapKeycode SDL.KeycodeB = KeyB
mapKeycode SDL.KeycodeC = KeyC
mapKeycode SDL.KeycodeD = KeyD
mapKeycode SDL.KeycodeE = KeyE
mapKeycode SDL.KeycodeF = KeyF
mapKeycode SDL.KeycodeG = KeyG
mapKeycode SDL.KeycodeH = KeyH
mapKeycode SDL.KeycodeI = KeyI
mapKeycode SDL.KeycodeJ = KeyJ
mapKeycode SDL.KeycodeK = KeyK
mapKeycode SDL.KeycodeL = KeyL
mapKeycode SDL.KeycodeM = KeyM
mapKeycode SDL.KeycodeN = KeyN
mapKeycode SDL.KeycodeO = KeyO
mapKeycode SDL.KeycodeP = KeyP
mapKeycode SDL.KeycodeQ = KeyQ
mapKeycode SDL.KeycodeR = KeyR
mapKeycode SDL.KeycodeS = KeyS
mapKeycode SDL.KeycodeT = KeyT
mapKeycode SDL.KeycodeU = KeyU
mapKeycode SDL.KeycodeV = KeyV
mapKeycode SDL.KeycodeW = KeyW
mapKeycode SDL.KeycodeX = KeyX
mapKeycode SDL.KeycodeY = KeyY
mapKeycode SDL.KeycodeZ = KeyZ
mapKeycode SDL.KeycodeCapsLock = KeyCapsLock
mapKeycode SDL.KeycodeF1 = KeyF1
mapKeycode SDL.KeycodeF2 = KeyF2
mapKeycode SDL.KeycodeF3 = KeyF3
mapKeycode SDL.KeycodeF4 = KeyF4
mapKeycode SDL.KeycodeF5 = KeyF5
mapKeycode SDL.KeycodeF6 = KeyF6
mapKeycode SDL.KeycodeF7 = KeyF7
mapKeycode SDL.KeycodeF8 = KeyF8
mapKeycode SDL.KeycodeF9 = KeyF9
mapKeycode SDL.KeycodeF10 = KeyF10
mapKeycode SDL.KeycodeF11 = KeyF11
mapKeycode SDL.KeycodeF12 = KeyF12
mapKeycode SDL.KeycodePrintScreen = KeyPrintScreen
mapKeycode SDL.KeycodeScrollLock = KeyScrollLock
mapKeycode SDL.KeycodePause = KeyPause
mapKeycode SDL.KeycodeInsert = KeyInsert
mapKeycode SDL.KeycodeHome = KeyHome
mapKeycode SDL.KeycodePageUp = KeyPageUp
mapKeycode SDL.KeycodeDelete = KeyDelete
mapKeycode SDL.KeycodeEnd = KeyEnd
mapKeycode SDL.KeycodePageDown = KeyPageDown
mapKeycode SDL.KeycodeRight = KeyRight
mapKeycode SDL.KeycodeLeft = KeyLeft
mapKeycode SDL.KeycodeDown = KeyDown
mapKeycode SDL.KeycodeUp = KeyUp
mapKeycode SDL.KeycodeNumLockClear = KeyNumLockClear
mapKeycode SDL.KeycodeKPDivide = KeyKPDivide
mapKeycode SDL.KeycodeKPMultiply = KeyKPMultiply
mapKeycode SDL.KeycodeKPMinus = KeyKPMinus
mapKeycode SDL.KeycodeKPPlus = KeyKPPlus
mapKeycode SDL.KeycodeKPEnter = KeyKPEnter
mapKeycode SDL.KeycodeKP1 = KeyKP1
mapKeycode SDL.KeycodeKP2 = KeyKP2
mapKeycode SDL.KeycodeKP3 = KeyKP3
mapKeycode SDL.KeycodeKP4 = KeyKP4
mapKeycode SDL.KeycodeKP5 = KeyKP5
mapKeycode SDL.KeycodeKP6 = KeyKP6
mapKeycode SDL.KeycodeKP7 = KeyKP7
mapKeycode SDL.KeycodeKP8 = KeyKP8
mapKeycode SDL.KeycodeKP9 = KeyKP9
mapKeycode SDL.KeycodeKP0 = KeyKP0
mapKeycode SDL.KeycodeKPPeriod = KeyKPPeriod
mapKeycode SDL.KeycodeApplication = KeyApplication
mapKeycode SDL.KeycodePower = KeyPower
mapKeycode SDL.KeycodeKPEquals = KeyKPEquals
mapKeycode SDL.KeycodeF13 = KeyF13
mapKeycode SDL.KeycodeF14 = KeyF14
mapKeycode SDL.KeycodeF15 = KeyF15
mapKeycode SDL.KeycodeF16 = KeyF16
mapKeycode SDL.KeycodeF17 = KeyF17
mapKeycode SDL.KeycodeF18 = KeyF18
mapKeycode SDL.KeycodeF19 = KeyF19
mapKeycode SDL.KeycodeF20 = KeyF20
mapKeycode SDL.KeycodeF21 = KeyF21
mapKeycode SDL.KeycodeF22 = KeyF22
mapKeycode SDL.KeycodeF23 = KeyF23
mapKeycode SDL.KeycodeF24 = KeyF24
mapKeycode SDL.KeycodeExecute = KeyExecute
mapKeycode SDL.KeycodeHelp = KeyHelp
mapKeycode SDL.KeycodeMenu = KeyMenu
mapKeycode SDL.KeycodeSelect = KeySelect
mapKeycode SDL.KeycodeStop = KeyStop
mapKeycode SDL.KeycodeAgain = KeyAgain
mapKeycode SDL.KeycodeUndo = KeyUndo
mapKeycode SDL.KeycodeCut = KeyCut
mapKeycode SDL.KeycodeCopy = KeyCopy
mapKeycode SDL.KeycodePaste = KeyPaste
mapKeycode SDL.KeycodeFind = KeyFind
mapKeycode SDL.KeycodeMute = KeyMute
mapKeycode SDL.KeycodeVolumeUp = KeyVolumeUp
mapKeycode SDL.KeycodeVolumeDown = KeyVolumeDown
mapKeycode SDL.KeycodeKPComma = KeyComma
mapKeycode SDL.KeycodeKPEqualsAS400 = KeyKPEqualsAS400
mapKeycode SDL.KeycodeAltErase = KeyAltErase
mapKeycode SDL.KeycodeSysReq = KeySysReq
mapKeycode SDL.KeycodeCancel = KeyCancel
mapKeycode SDL.KeycodeClear = KeyClear
mapKeycode SDL.KeycodePrior = KeyPrior
mapKeycode SDL.KeycodeReturn2 = KeyReturn2
mapKeycode SDL.KeycodeSeparator = KeySeparator
mapKeycode SDL.KeycodeOut = KeyOut
mapKeycode SDL.KeycodeOper = KeyOper
mapKeycode SDL.KeycodeClearAgain = KeyClearAgain
mapKeycode SDL.KeycodeCrSel = KeyCrSel
mapKeycode SDL.KeycodeExSel = KeyExSel
mapKeycode SDL.KeycodeKP00 = KeyKP00
mapKeycode SDL.KeycodeKP000 = KeyKP000
mapKeycode SDL.KeycodeThousandsSeparator = KeyThousandsSeparator
mapKeycode SDL.KeycodeDecimalSeparator = KeyDecimalSeparator
mapKeycode SDL.KeycodeCurrencyUnit = KeyCurrencyUnit
mapKeycode SDL.KeycodeCurrencySubunit = KeyCurrencySubunit
mapKeycode SDL.KeycodeKPLeftParen = KeyKPLeftParen
mapKeycode SDL.KeycodeKPRightParen = KeyKPRightParen
mapKeycode SDL.KeycodeKPLeftBrace = KeyKPLeftBrace
mapKeycode SDL.KeycodeKPRightBrace = KeyKPRightBrace
mapKeycode SDL.KeycodeKPTab = KeyKPTab
mapKeycode SDL.KeycodeKPBackspace = KeyKPBackspace
mapKeycode SDL.KeycodeKPA = KeyKPA
mapKeycode SDL.KeycodeKPB = KeyKPB
mapKeycode SDL.KeycodeKPC = KeyKPC
mapKeycode SDL.KeycodeKPD = KeyKPD
mapKeycode SDL.KeycodeKPE = KeyKPE
mapKeycode SDL.KeycodeKPF = KeyKPF
mapKeycode SDL.KeycodeKPXor = KeyKPXor
mapKeycode SDL.KeycodeKPPower = KeyKPPower
mapKeycode SDL.KeycodeKPPercent = KeyKPPercent
mapKeycode SDL.KeycodeKPLess = KeyKPLess
mapKeycode SDL.KeycodeKPGreater = KeyKPGreater
mapKeycode SDL.KeycodeKPAmpersand = KeyKPAmpersand
mapKeycode SDL.KeycodeKPDblAmpersand = KeyKPDblAmpersand
mapKeycode SDL.KeycodeKPVerticalBar = KeyKPVerticalBar
mapKeycode SDL.KeycodeKPDblVerticalBar = KeyKPDblVerticalBar
mapKeycode SDL.KeycodeKPColon = KeyKPColon
mapKeycode SDL.KeycodeKPHash = KeyKPHash
mapKeycode SDL.KeycodeKPSpace = KeyKPSpace
mapKeycode SDL.KeycodeKPAt = KeyKPAt
mapKeycode SDL.KeycodeKPExclam = KeyKPExclam
mapKeycode SDL.KeycodeKPMemStore = KeyKPMemStore
mapKeycode SDL.KeycodeKPMemRecall = KeyKPMemRecall
mapKeycode SDL.KeycodeKPMemClear = KeyKPMemClear
mapKeycode SDL.KeycodeKPMemAdd = KeyKPMemAdd
mapKeycode SDL.KeycodeKPMemSubtract = KeyKPMemSubtract
mapKeycode SDL.KeycodeKPMemMultiply = KeyKPMemMultiply
mapKeycode SDL.KeycodeKPMemDivide = KeyKPMemDivide
mapKeycode SDL.KeycodeKPPlusMinus = KeyKPPlusMinus
mapKeycode SDL.KeycodeKPClear = KeyKPClear
mapKeycode SDL.KeycodeKPClearEntry = KeyKPClearEntry
mapKeycode SDL.KeycodeKPBinary = KeyKPBinary
mapKeycode SDL.KeycodeKPOctal = KeyKPOctal
mapKeycode SDL.KeycodeKPDecimal = KeyKPDecimal
mapKeycode SDL.KeycodeKPHexadecimal = KeyKPHexadecimal
mapKeycode SDL.KeycodeLCtrl = KeyLCtrl
mapKeycode SDL.KeycodeLShift = KeyLShift
mapKeycode SDL.KeycodeLAlt = KeyLAlt
mapKeycode SDL.KeycodeLGUI = KeyLGUI
mapKeycode SDL.KeycodeRCtrl = KeyRCtrl
mapKeycode SDL.KeycodeRShift = KeyRShift
mapKeycode SDL.KeycodeRAlt = KeyRAlt
mapKeycode SDL.KeycodeRGUI = KeyRGUI
mapKeycode SDL.KeycodeMode = KeyMode
mapKeycode SDL.KeycodeAudioNext = KeyAudioNext
mapKeycode SDL.KeycodeAudioPrev = KeyAudioPrev
mapKeycode SDL.KeycodeAudioStop = KeyAudioStop
mapKeycode SDL.KeycodeAudioPlay = KeyAudioPlay
mapKeycode SDL.KeycodeAudioMute = KeyAudioMute
mapKeycode SDL.KeycodeMediaSelect = KeyMediaSelect
mapKeycode SDL.KeycodeWWW = KeyWWW
mapKeycode SDL.KeycodeMail = KeyMail
mapKeycode SDL.KeycodeCalculator = KeyCalculator
mapKeycode SDL.KeycodeComputer = KeyComputer
mapKeycode SDL.KeycodeACSearch = KeyACSearch
mapKeycode SDL.KeycodeACHome = KeyACHome
mapKeycode SDL.KeycodeACBack = KeyACBack
mapKeycode SDL.KeycodeACForward = KeyACForward
mapKeycode SDL.KeycodeACStop = KeyACStop
mapKeycode SDL.KeycodeACRefresh = KeyACRefresh
mapKeycode SDL.KeycodeACBookmarks = KeyACBookmarks
mapKeycode SDL.KeycodeBrightnessDown = KeyBrightnessDown
mapKeycode SDL.KeycodeBrightnessUp = KeyBrightnessUp
mapKeycode SDL.KeycodeDisplaySwitch = KeyDisplaySwitch
mapKeycode SDL.KeycodeKbdIllumToggle = KeyKbdIllumToggle
mapKeycode SDL.KeycodeKbdIllumDown = KeyKbdIllumDown
mapKeycode SDL.KeycodeKbdIllumUp = KeyKbdIllumUp
mapKeycode SDL.KeycodeEject = KeyEject
mapKeycode SDL.KeycodeSleep = KeySleep
mapKeycode SDL.KeycodeUnknown = KeyUnknown
mapKeycode _ = KeyUnknown