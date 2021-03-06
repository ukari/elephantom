{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
-- record field
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- type
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- sugar
--{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- foreign
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Text.InterpolatedString.QM (qnb)
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan
import qualified Vulkan.Core10 as Core10
import qualified Vulkan.Extensions.VK_KHR_swapchain as Swap
import Vulkan.Extensions.VK_EXT_acquire_xlib_display
import Vulkan.Exception
--import Vulkan.Utils.Debug
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import Vulkan.Utils.Initialization (createDebugInstanceFromRequirements, createDeviceFromRequirements)
import Vulkan.Requirement
import qualified VulkanMemoryAllocator as Vma
import Codec.Picture (PixelRGBA8 (..), readImage, imageData)
import qualified Codec.Picture as JP
import Graphics.Rasterific (renderDrawing, rectangle, fill)
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Text.TrueType (RawGlyph (..), Font, loadFontFile, decodeFont, descriptorOf, _descriptorFamilyName, getCharacterGlyphsAndMetrics, unitsPerEm)
import FreeType

import Language.Haskell.TH hiding (location)
import Type.Reflection (SomeTypeRep, splitApps, typeOf)
import GHC.Generics (Generic)
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr)
import Foreign.Storable (Storable (sizeOf, alignment))
import qualified Foreign.Storable as Storable
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Utils (copyBytes, with, fillBytes)
import Foreign.Marshal.Array (peekArray)

import Linear ((!*!), V2 (..), V3 (..), V4 (..), M44, Quaternion (..), Epsilon, transpose, identity, lookAt, ortho, inverseOrtho, mkTransformation, axisAngle, m33_to_m44, scaled)
import qualified Linear
--import Data.Acquire (Acquire, mkAcquire)
import Data.Char (digitToInt)
import Data.String (IsString)
import Data.Word (Word32)
import Data.Int (Int16)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.ByteString (packCString, pack)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Traversable (traverse)
import Data.Bits ((.&.), (.|.), shift, zeroBits)
import Data.Vector ((!), (!?), uniq, modify)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
--import Data.Vector.Algorithms.Intro (sort)
import qualified Data.Vector.Algorithms.Intro as V
--import Data.Set (Set, union)
--import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Bool (bool)
import Data.List ((\\), group, sort, sortOn, groupBy)
import Data.Function (on)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Functor ((<&>))
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.StateVar as StateVar
import System.Mem.Weak
import System.Mem.StableName
import Numeric (showHex)

import Streamly
import Streamly.Prelude (drain, yield, repeatM)
import qualified Streamly.Prelude as S
import Reflex
import qualified Reflex as R
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import Reflex.Host.Class (MonadReflexHost, runHostFrame, fireEventsAndRead, fireEvents)
import Reflex.Workflow (Workflow (..))
import Reflex.Network (networkHold)
import Lifetimes (Acquire (..), mkAcquire, withAcquire, releaseEarly, detach)
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict hiding (modify)
--import Control.Carrier.Error.Church (runError)
--import Control.Effect.Throw (throwError)
--import Control.Effect.Exception (throw, catch, catchJust, try, tryJust)
import Control.Arrow ((&&&))
import Control.Applicative ((<|>), Applicative (..), optional, liftA, liftA2)
import Control.Monad (liftM, liftM2, join, forever, forM)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, ReleaseKey, runResourceT, allocate, allocate_, release, register, unprotect, liftResourceT, resourceForkIO, transResourceT)
import Control.Monad.Fix (MonadFix)
import Control.Exception (Exception (..), SomeException (..), AsyncException (UserInterrupt), throw, handleJust, try)
import qualified Control.Exception as Ex
import Control.Concurrent (MVar, newMVar, readMVar, forkIO, forkOS, threadDelay, myThreadId)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Data.Functor.Identity (runIdentity)

import Control.Algebra (Has)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (ThrowC (..), runThrow)
import Control.Carrier.Error.Either (ErrorC (..), runError)
import Control.Effect.Lift (Lift (..))
import Control.Effect.Writer (Writer (..), tell, run)
import Control.Effect.Error (Error)
import Control.Effect.Throw (Throw (..), throwError, liftEither)
import Control.Effect.Catch (Catch (..), catchError)
--import Control.Carrier.State.IORef (runState)
import Control.Effect.Exception (throwIO, catch)

import Control.Monad.Logger (runStdoutLoggingT)
-- import Control.Concurrent
import Control.Concurrent.KazuraQueue (newQueue)
import qualified Control.Concurrent.KazuraQueue as KazuraQueue
import Event
import Listen
import GLSL
import Shader
import Offset
import qualified SpirV
import Elephantom.Renderer
import qualified Elephantom.Renderer as Lib
import qualified Elephantom.Renderer.Wrapper as Lib
import Elephantom.Application (Application (..), defaultApplication)
import Acquire (Acquire, Cleaner, MonadCleaner, mkAcquire, acquireT, cleanup, collect)

import Elephantom.Renderer.RendererException

import System.FilePath ((</>), takeDirectory, takeFileName, equalFilePath, makeRelative, normalise)
import System.Directory (canonicalizePath)
import System.Environment (getProgName, getExecutablePath)
import Paths_elephantom (getDataFileName, getBinDir, getDataDir, getSysconfDir)

testEff :: IO (Either RendererException ())
testEff = runError testThrowException

testThrowException :: ( Has (Error RendererException :+: Lift IO) sig m
                      , Has (Lift IO) sig m
                      , MonadIO m
                      ) => m ()
testThrowException = do
  liftIO $ print "hi1"
  sendIO $ print "hi2"
  sendM $ print "hi3"
  x <- throwError VulkanDeviceNotFound
  return ()

data ExecuteEnviornmentType = Ghci | Exe deriving (Show)

data ExecuteEnviornment = ExecuteEnviornment
  { executableDirectory :: FilePath
  , executableType :: ExecuteEnviornmentType
  } deriving (Show)

checkExecuteEnviornmentType :: String -> String -> ExecuteEnviornmentType
checkExecuteEnviornmentType "<interactive>" "ghc" = Ghci
checkExecuteEnviornmentType _ _ = Exe

getEnviornment :: IO ExecuteEnviornment
getEnviornment = do
  progName <- getProgName
  executablePath <- getExecutablePath
  let executableDirectory = takeDirectory executablePath
  let executableFileName = takeFileName executablePath
  let executableType = checkExecuteEnviornmentType progName executableFileName
  pure ExecuteEnviornment { .. }

getBasePath :: ExecuteEnviornment -> IO FilePath
getBasePath (ExecuteEnviornment executableDirectory Exe) = pure executableDirectory
getBasePath (ExecuteEnviornment _ Ghci) = getBinDir

data AppConfig = AppConfig
  { binPath :: FilePath
  , dataPath :: FilePath
  } deriving (Show)

type App sig m = ( Has (State AppConfig) sig m
                 , MonadIO m
                 )

-- todo
-- make a search list for data and log which in use
detectAppConfig :: ExecuteEnviornment -> IO AppConfig
detectAppConfig (ExecuteEnviornment executableDirectory Exe) = do
  let binPath = executableDirectory
  let basePath = takeDirectory binPath
  binInstallPath <- getBinDir
  dataInstallPath <- getDataDir
  relBinPath <- canonicalizePath binPath
  relBinInstallPath <- canonicalizePath binInstallPath
  let dataPath = if relBinPath `equalFilePath` relBinInstallPath then dataInstallPath else basePath </> "data"
  pure AppConfig { .. }
detectAppConfig (ExecuteEnviornment _ Ghci) = do
  binPath <- getBinDir
  dataPath <- getDataDir
  pure AppConfig {..}

runTest2 :: IO ()
runTest2 = runTestApp

runTestApp :: forall sig m . (Algebra sig m, MonadIO m) => m ()
runTestApp = do
  env <- liftIO getEnviornment
  appConfig <- liftIO $ detectAppConfig env
  liftIO $ print appConfig
  --evalState appConfig testfont
  evalState appConfig testfreetype

testAppCon :: App sig m => m ()
testAppCon = do
  config <- binPath <$> get @AppConfig
  liftIO $ print config
  liftIO $ print @String "hi"
  pure ()

testfreetype :: App sig m => m ()
testfreetype = do
  dataBasePath <- dataPath <$> get @AppConfig
  let fontfile = dataBasePath </> "font/NotoSansCJKsc-Regular.ttf"--"font/SourceCodePro-Regular.ttf"--"font/NotoSansMonoCJKsc-Regular.otf"
  liftIO $ print fontfile
  ft_library <- liftIO ft_Init_FreeType
  ft_face <- liftIO . ft_New_Face ft_library fontfile $ 0
  liftIO $ print $ "ft_face: " <> show ft_face
  face <- liftIO $ peek ft_face
  let ft_glyph_slot = frGlyph face
  --slot <- liftIO $ peek ft_glyph_slot
  let unitsPerEm = frUnits_per_EM face
  liftIO $ print $ "unitsPerEm" <> show unitsPerEm
  --liftIO $ ft_Set_Char_Size ft_face 0 (16*64) 300 300
  liftIO $ ft_Set_Pixel_Sizes ft_face (fromIntegral unitsPerEm) (fromIntegral unitsPerEm)
  let charcode = fromIntegral . fromEnum $ '???'
  glyph_index <- liftIO . ft_Get_Char_Index ft_face $ charcode
  liftIO $ print $ "glyph index: " <> show glyph_index
  liftIO $ ft_Load_Glyph ft_face glyph_index FT_LOAD_NO_BITMAP
  slot <- liftIO $ peek ft_glyph_slot
  let advance = gsrAdvance slot
  liftIO $ print $ "x advance: " <> show (vX advance `div` 64)
  liftIO $ print $ "y advance: " <> show (vY advance `div` 64)
  let format = gsrFormat slot
  liftIO $ print $ "gsrFormat: " <> show format
  let outline = gsrOutline slot
  ft_bbox <- liftIO . with outline $ \ft_outline -> ft_Outline_Get_BBox ft_outline
  liftIO $ print $ "bbXMin: " <> show (bbXMin ft_bbox `div` 64)
  liftIO $ print $ "bbYMin: " <> show (bbYMin ft_bbox `div` 64)
  liftIO $ print $ "bbXMax: " <> show (bbXMax ft_bbox `div` 64)
  liftIO $ print $ "bbYMax: " <> show (bbYMax ft_bbox `div` 64)
  let contoursN = oN_contours outline
  liftIO $ print $ "contoursN: " <> show contoursN
  contours <- liftIO $ peekArray (fromIntegral contoursN) (oContours outline)
  liftIO $ print contours
  let pointsN = oN_points outline
  points <- liftIO $ peekArray (fromIntegral pointsN) (oPoints outline)
  liftIO $ print $ "pointsN: " <> show pointsN
  liftIO $ print $ map (\p -> (vX p `div` 64, vY p `div` 64)) points
  let controlN = gsrControl_len slot
  liftIO $ print $ "controlN: " <> show controlN
  tags <- liftIO $ peekArray (fromIntegral pointsN) (oTags outline)
  liftIO $ print $ "tags: " <> show tags

  liftIO $ ft_Done_Face ft_face
  liftIO $ ft_Done_FreeType ft_library
  pure ()

testfont :: App sig m => m ()
testfont = do
  dataBasePath <- dataPath <$> get @AppConfig
  let fontfile = dataBasePath </> "font/NotoSansCJKsc-Regular.ttf"--"font/NotoSerif-Regular.ttf"--"font/SourceCodePro-Regular.ttf" -- "font/NotoSansMonoCJKsc-Regular.otf"
  liftIO $ print fontfile
  res <- liftIO $ loadFontFile fontfile
  liftIO $ case res of
    Left e -> print $ "fail to load font " <> e
    Right font -> do
      print $ ("load font " <>) . show <$> descriptorOf font
      loadfont font
  liftIO $ print "test font"

loadfont :: Font -> IO ()
loadfont font = do
  let (advance, glyphs) = getCharacterGlyphsAndMetrics font '???'
  print $ "advance: " <> show advance
  forM glyphs $ \(RawGlyph scales index contours) -> do
    print $ "scales: " <> show scales
    print $ "index: " <> show index
    print $ "contours: " <> show contours
    print $ "contours points count: " <> show (VU.length <$> contours)
    print $ "unitsPerEm: " <> show (unitsPerEm font)
  pure ()

testRelease :: IO ()
testRelease = runResourceT $ do
  key <- register (print "hi")
  release key
  liftIO . print $ "end"

type Signal t m = (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))

type Varing t m = (Reflex t, MonadHold t m, MonadFix m)

eventr :: (Signal t m, MonadIO m) => m (R.Event t TickInfo)
eventr = do
  cur <- liftIO getCurrentTime
  tickLossy 6 cur

ticker :: (Signal t m, MonadIO m, Integral a) => a -> m (R.Event t TickInfo)
ticker duration = do
  cur <- liftIO getCurrentTime
  tickLossy (fromIntegral duration) cur

ticker' :: (Signal t m, MonadIO m, Integral a) => a -> m (R.Event t TickInfo)
ticker' duration = do
  cur <- liftIO getCurrentTime
  d <- clockLossy (fromIntegral duration) cur
  pure . updated $ d

tick :: (Signal t m, MonadIO m, Integral a, Adjustable t m) => Dynamic t a -> m (R.Event t TickInfo)
tick durationD = do
  be <- snd <$> runWithReplace (pure ()) (ticker <$> updated durationD)
  initial <- R.sample . current $ durationD
  initialE <- ticker initial
  switchHold initialE be

tick' :: (Signal t m, MonadIO m, Integral a, Adjustable t m) => Dynamic t a -> m (R.Event t TickInfo)
tick' durationD = do
  initial <- R.sample . current $ durationD
  be <- networkHold (ticker initial) (ticker <$> updated durationD)
  pure . switchDyn $ be

test :: IO ()
test = runHeadlessApp $ do
  (tickConfigEvent, tickConfigTrigger) <- newTriggerEvent
  e <- eventr
  dy <- foldDyn (\a b -> ((a + b) `mod` 3) + 1) 0 (1 <$ e)
  tickDyn <- holdDyn 1 tickConfigEvent
  te <- switchDynamic tickDyn ticker'
  performEvent_ $ (\info -> liftIO (print info >> (print =<< getCurrentTime) >> print "frame start" >> threadDelay 2100000 >> (print =<< getCurrentTime) >> print "frame end") ) <$> te
  postBuild <- getPostBuild
  -- performEvent_ $ liftIO . tickConfigTrigger <$> traceEvent "hi" (updated dy)
 -- performEvent_ $ liftIO . print <$> leftmost [postBuild, () <$ te]
  pure never

switchDynamic :: (Signal t m, MonadIO m, Integral a, Adjustable t m) => Dynamic t a -> (a -> m (R.Event t b)) -> m (R.Event t b)
switchDynamic d f = do
  initial <- R.sample . current $ d
  initialE <- f initial
  e <- snd <$> runWithReplace (pure ()) (f <$> updated d)
  switchHold initialE e



data EvenException = EvenException deriving (Show)

instance Exception EvenException

makeRes :: (Show a, MonadResource m) => a -> m a
makeRes x = do
  _ <- allocate_ (liftIO . pure $ x) (liftIO . print $ "release " <> show x)
  pure x

step :: (MonadIO m) => (String, Int) -> m (Maybe (String, Int))
step (name, value) = do
  if odd value
    then throw EvenException
    else if value < 10
         then do
           liftIO . print $ name <> " is : " <> show value
           pure . Just $ (name, value + 1)
         else pure Nothing

makeResource :: (MonadResource m) => String -> Int -> m (String, Int)
makeResource name value = do
  name <- makeRes "a"
  value <- makeRes 0
  pure (name, value)

testRes :: IO ()
testRes = do
  let fps = 1
  resCont $ liftIO . S.drainWhile isJust . S.drop 1 . asyncly . constRate fps . S.iterateM (maybe (pure Nothing) step) . pure . Just

resCont :: MonadUnliftIO m => ((String, Int) -> ResourceT m a) -> m a
resCont g = runResourceT $ do
  res <- makeResource "a" 0
  g res

testResH :: (Show r1) => r1 -> (String -> IO ()) -> IO ()
testResH x f = runResourceT $ do
  liftIO $ print $ "use " <> show x
  rtmp <- makeRes ("tmp"::String)
  liftIO . f $ rtmp
  liftIO . print $ "end use " <> show rtmp

testRes2 :: IO ()
testRes2 = runResourceT $ do
  r1 <- makeRes 1
  r2 <- makeRes 2
  liftIO $ testResH r1 $ \rtmp ->
    print $ "use" <> show rtmp
  pure ()

ortho' :: (Num a, Floating a) => a -> a -> a -> a -> a -> a -> M44 a
ortho' left right bottom top near far = V4
  (V4 (2/(right - left)) 0 0 (-(right + left)/(right - left)))
  (V4 0 (2/(top - bottom)) 0 (-(top + bottom)/(top - bottom)))
  (V4 0 0 (-2/(far - near)) (-(far + near)/(far - near)))
  (V4 0 0 0 1)

ortho2D :: (Num a, Floating a) => a -> a -> a -> a -> M44 a
ortho2D left right bottom top = ortho' left right bottom top (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))

rotateAt :: (Num a, Epsilon a, Floating a) => V3 a -> Quaternion a -> M44 a
rotateAt (V3 x y z) quaternion = mkTransformation quaternion (V3 (0+(x)) (0+(y)) (0+(z))) !*! mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 (-x) (-y) (-z))

foreign import ccall "test_add" test_foreign_add :: Int -> Int -> IO Int

someFunc :: IO ()
someFunc = runResourceT $ do
  fres <- liftIO $ test_foreign_add 1 2
  liftIO $ print fres
  let application@Application {..} = defaultApplication { vkDebug = False, width = 1000, height = 1000, fps = 12, bgRed = 255, bgGreen = 255, bgBlue = 255 } :: Application

  eventQueue <- liftIO newQueue
  withSDL
  window <- snd <$> withWindow "test" width height allocate
  _ <- liftIO . forkIO . drain . asyncly $
    parallel
      (constRate (fromIntegral inputRate) . repeatM . liftIO . eventLoop $ eventQueue)
      (repeatM . runStdoutLoggingT . listenLoop $ eventQueue)
  inst <- withInst application window
  surf <- snd <$> withSurface inst window allocate
  (vendorID, phys) <- getPhysicalDevice inst
  liftIO . print . maxBoundDescriptorSets . limits =<< getPhysicalDeviceProperties phys
  liftIO . print $ ("vendorID: 0x" <> showHex vendorID "")
  qIndices <- findQueueFamilyIndices phys surf
  liftIO $ print qIndices
  let queueFamilyIndices = uniq . modify V.sort $ fmap ($ qIndices) [ graphicsFamily, presentFamily, transferFamily ]
  device <- Lib.withDevice phys queueFamilyIndices
  liftIO $ print $ "device " <> show (deviceHandle device)
  queueRes@QueueResource {..} <- getQueueResource device qIndices
  liftIO $ print $ queueHandle graphicsQueue
  liftIO $ print $ queueHandle presentQueue
  liftIO $ print $ queueHandle transferQueue
  commandPoolRes@CommandPoolResource {..} <- snd <$> withCommandPoolResource device qIndices allocate
  formats <- snd <$> getPhysicalDeviceSurfaceFormatsKHR phys surf
  let surfaceFormat = chooseFormat formats
  liftIO $ print formats
  liftIO $ print surfaceFormat
  renderPass <- snd <$> Lib.withRenderPass device surfaceFormat allocate

  liftIO . print =<< getPhysicalDeviceSurfaceCapabilitiesKHR phys surf

  -- resource load
  -- https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/vk__mem__alloc_8h.html#a4f87c9100d154a65a4ad495f7763cf7c
  allocator <- snd <$> Lib.withAllocator phys device inst allocate

  (shaderRes, shaderModules) <- withShaderStages device
  liftIO . print $ shaderModules
  pipelineRes <- snd <$> Lib.withPipelineResource device renderPass shaderRes defaultPipelineCreateInfo allocate
  mapM_ (Lib.destroyShaderModule device) shaderModules
  (triangleCleaner, trianglePresent) <- liftIO . collect $ loadTriangle application allocator device queueFamilyIndices shaderRes pipelineRes
  Lib.destroyShaderResource device shaderRes

  (textureShaderRes, textureShaderModules) <- Lib.withTextureShaderStages device
  texturePipelineRes <- snd <$> Lib.withPipelineResource device renderPass textureShaderRes defaultPipelineCreateInfo allocate
  liftIO . print $ textureShaderModules
  mapM_ (Lib.destroyShaderModule device) textureShaderModules
  (textureCleaner, texturePresent) <- liftIO . collect $ loadTexture application allocator phys device queueFamilyIndices queueRes commandPoolRes textureShaderRes texturePipelineRes
  Lib.destroyShaderResource device textureShaderRes
  
  (contoursShaderRes, contoursShaderModules) <- Lib.withContoursShaderStages device
  contoursPipelineRes <- snd <$> Lib.withPipelineResource device renderPass contoursShaderRes defaultPipelineCreateInfo allocate
  mapM_ (Lib.destroyShaderModule device) contoursShaderModules
  (contoursCleaner, contoursPresent) <- liftIO . collect $ loadContours application allocator phys device queueFamilyIndices queueRes commandPoolRes contoursShaderRes contoursPipelineRes
  Lib.destroyShaderResource device contoursShaderRes
  -- resource load end

  V2 windowWidth windowHeight <- SDL.vkGetDrawableSize window
  let fallbackExtent = Extent2D (fromIntegral windowWidth) (fromIntegral windowHeight)
  swapchainRes@SwapchainResource {..} <- createSwapchain phys device surf surfaceFormat queueFamilyIndices fallbackExtent renderPass NULL_HANDLE

  -- mapM_ (submitCommand extent renderPass [ trianglePresent, texturePresent ]) (V.zip commandBuffers framebuffers)
  presentsMVar <- liftIO . newMVar $ [ trianglePresent, texturePresent, contoursPresent ]
  frameSync@FrameSync {..} <- createFrameSync device framebuffers

  commandBufferRes@CommandBufferResource {..} <- createCommandBufferResource device graphicsCommandPool frameSize
  cleanupMVar <- liftIO . newMVar $ (frameSync, commandBufferRes, swapchainRes)
  let ctx = Context {..}
  -- liftIO . flip Ex.finally (cleanupFrame device cleanupMVar >> foldMap cleanup ([ triangleCleaner, textureCleaner , contoursCleaner ] :: [ Cleaner ])) . S.drainWhile isJust . S.drop 1 . asyncly . S.iterateM (maybe (pure Nothing) drawFrame) . pure . Just $ (ctx, frameSync, commandBufferRes, swapchainRes)
  liftIO . flip Ex.finally (cleanupFrame device cleanupMVar >> foldMap cleanup ([ triangleCleaner, textureCleaner, contoursCleaner ] :: [ Cleaner ])) . S.drainWhile isJust . S.drop 1 . asyncly . minRate (fromIntegral fps) . maxRate (fromIntegral fps) . S.iterateM (maybe (pure Nothing) drawFrame) . pure . Just $ (ctx, frameSync, commandBufferRes, swapchainRes)

loadTriangle :: MonadCleaner m => Application -> Vma.Allocator -> Device -> V.Vector Word32 -> ShaderResource -> PipelineResource -> m Present
loadTriangle Application { width, height } allocator device queueFamilyIndices shaderRes pipelineRes = do

  (vertexBuffer, vertexBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ 3 * sizeOf (undefined :: ShaderInputVertex)
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT -- support vkCmdBindVertexBuffers.pBuffers
    , sharingMode = SHARING_MODE_EXCLUSIVE -- chooseSharingMode queueFamilyIndices
    -- , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU--GPU_ONLY
    } acquireT
  let vertices =
        [ ShaderInputVertex (V2 250 125) (V4 (102/255) (53/255) (53/255) 1)
        , ShaderInputVertex (V2 375 (375)) (V4 (53/255) (102/255) (53/255) 1)
        , ShaderInputVertex (V2 (125) (375)) (V4 (53/255) (53/255) (102/255) 1)
        ] :: VS.Vector ShaderInputVertex

  memCopyV allocator vertexBufferAllocation vertices -- early free

  let indices = [0, 1, 2] :: VS.Vector Word32
  (indexBuffer, indexBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (indices VS.! 0) * VS.length indices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopyV allocator indexBufferAllocation indices

  (uniformBuffer, uniformBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ 1 * sizeOf (undefined :: ShaderUniform)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode queueFamilyIndices
    , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  let uniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (fromIntegral width) (0) (fromIntegral height) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (500/2*2.5) (500/2*2.5) 0) (axisAngle (V3 0 0 1) (45/360*2*pi)) !*! (m33_to_m44 . scaled $ 2.5)
        }
  memCopy allocator uniformBufferAllocation uniform -- early free
  liftIO . print $ descriptorSetLayoutCreateInfos shaderRes
  descriptorSetResource <- Lib.withDescriptorSetResource device (descriptorSetLayouts shaderRes) (descriptorSetLayoutCreateInfos shaderRes) acquireT
  liftIO . print $ descriptorSetResource
  let bufferInfos :: V.Vector DescriptorBufferInfo
      bufferInfos =
        [ zero
          { buffer = uniformBuffer
          , offset = 0
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        ]
  updateDescriptorSets device
    [ SomeStruct $ zero
      { dstSet = descriptorSets (descriptorSetResource :: DescriptorSetResource) ! 2
      , dstBinding = 0
      , dstArrayElement = 0
      , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , descriptorCount = fromIntegral . length $ bufferInfos
      , bufferInfo = bufferInfos
      , imageInfo = []
      , texelBufferView = []
      }
    ] []
  pure Present
    { vertexBuffers = [ vertexBuffer ]
    , indexBuffer = indexBuffer
    , descriptorSets = descriptorSets (descriptorSetResource :: DescriptorSetResource)
    , drawSize = fromIntegral . VS.length $ indices
    , pipelineResource = pipelineRes
    }

loadTexture :: (MonadIO m, MonadCleaner m) => Application -> Vma.Allocator -> PhysicalDevice -> Device -> V.Vector Word32 -> QueueResource -> CommandPoolResource -> ShaderResource -> PipelineResource -> m Present
loadTexture Application { width, height } allocator phys device queueFamilyIndices QueueResource {..} CommandPoolResource {..}  textureShaderRes pipelineRes = do
  liftIO . print . descriptorSetLayouts $ textureShaderRes
  textureSampler <- Lib.withTextureSampler phys device acquireT
  textureDescriptorSetResource <- Lib.withDescriptorSetResource device (descriptorSetLayouts textureShaderRes) (descriptorSetLayoutCreateInfos textureShaderRes) acquireT
  let texCoords =
        [ Texture (V2 50 50) (V4 0 0 0 1) (V2 0 0)
        , Texture (V2 250 50) (V4 0 0 0 1) (V2 1 0)
        , Texture (V2 250 150) (V4 0 0 0 1) (V2 1 1)
        , Texture (V2 50 150) (V4 0 0 0 1) (V2 0 1)
        ] :: VS.Vector Texture
  (texCoordsBuffer, texCoordsBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (texCoords VS.! 0) * VS.length texCoords
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopyV allocator texCoordsBufferAllocation texCoords

  let texIndices = [0, 1, 2, 2, 3, 0] :: VS.Vector Word32
  (texIndexBuffer, texIndexBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (texIndices VS.! 0) * VS.length texIndices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopyV allocator texIndexBufferAllocation texIndices
  
  let texUniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (fromIntegral width) (0) (fromIntegral height) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (150/2*1) (100/2*1) 0) (axisAngle (V3 0 0 1) ((45+30)/360*2*pi)) !*! (m33_to_m44 . scaled $ 1)
        }
  (texUniformBuffer, texUniformBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ 4 * sizeOf (undefined :: ShaderUniform)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode queueFamilyIndices
    , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopyV allocator texUniformBufferAllocation (VS.fromList [texUniform, texUniform, texUniform, texUniform]) -- early free
  let texBufferInfos :: V.Vector DescriptorBufferInfo
      texBufferInfos =
        [ zero
          { buffer = texUniformBuffer
          , offset = 0
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        , zero
          { buffer = texUniformBuffer
          , offset = fromIntegral . (*1) . sizeOf $ (undefined :: ShaderUniform)
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        , zero
          { buffer = texUniformBuffer
          , offset = fromIntegral . (*2) . sizeOf $ (undefined :: ShaderUniform)
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        , zero
          { buffer = texUniformBuffer
          , offset = fromIntegral . (*3) . sizeOf $ (undefined :: ShaderUniform)
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        ]

  let pixels = renderDrawing 200 100 (PixelRGBA8 255 255 0 100) $ fill $ rectangle (V2 0 0) 200 100
  (textureStagingBuffer, textureStagingBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ (sizeOf . VS.head $ imageData pixels) * VS.length (imageData pixels)
    , usage = BUFFER_USAGE_TRANSFER_SRC_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_ONLY
    } acquireT
  memCopyV allocator textureStagingBufferAllocation (imageData pixels)
  let textureFormat = FORMAT_R8G8B8A8_SRGB
  (textureImage, _textureImageAllocation, _) <- Lib.withImage allocator zero
    { imageType = IMAGE_TYPE_2D
    , extent = Extent3D 200 100 1
    , mipLevels = 1
    , arrayLayers = 1
    , format = textureFormat
    , tiling = IMAGE_TILING_OPTIMAL
    , initialLayout = IMAGE_LAYOUT_UNDEFINED
    , usage = IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT -- when use staging buffer, VK_IMAGE_USAGE_TRANSFER_DST_BIT is necessary. VUID-VkImageMemoryBarrier-oldLayout-01213
    , samples = SAMPLE_COUNT_1_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_GPU_ONLY
    } acquireT

  withSingleTimeCommands device transferCommandPool transferQueue $ \cb -> do
    transitionImageLayout cb textureImage IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    cmdCopyBufferToImage cb textureStagingBuffer textureImage IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      [ zero
        { bufferOffset = 0
        , bufferRowLength = 0
        , bufferImageHeight = 0
        , imageSubresource =  zero
          { aspectMask = IMAGE_ASPECT_COLOR_BIT
          , mipLevel = 0
          , baseArrayLayer = 0
          , layerCount = 1
          }
        , imageOffset = Offset3D 0 0 0
        , imageExtent = Extent3D (fromIntegral . JP.imageWidth $ pixels) (fromIntegral . JP.imageHeight $ pixels) 1
        } :: BufferImageCopy
      ]
    transitionImageLayout cb textureImage IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  textureImageView <- Lib.withImageView device textureFormat textureImage acquireT
  updateDescriptorSets device
    [ SomeStruct $ zero
      { dstSet = descriptorSets (textureDescriptorSetResource :: DescriptorSetResource) ! 2
      , dstBinding = 0
      , dstArrayElement = 0
      , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , descriptorCount = fromIntegral . length $ texBufferInfos
      , bufferInfo = texBufferInfos
      , imageInfo = []
      , texelBufferView = []
      }
    , SomeStruct $ zero
      { dstSet = descriptorSets (textureDescriptorSetResource :: DescriptorSetResource) ! 2
      , dstBinding = 1
      , dstArrayElement = 0
      , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , descriptorCount = 1
      , imageInfo =
        [ zero
          { imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          , imageView = textureImageView
          , sampler = textureSampler
          }
        ]
      }
    ] []
  pure Present
    { vertexBuffers = [ texCoordsBuffer ]
    , indexBuffer = texIndexBuffer
    , descriptorSets = descriptorSets (textureDescriptorSetResource :: DescriptorSetResource)
    , drawSize = fromIntegral . VS.length $ texIndices
    , pipelineResource = pipelineRes
    }

bContours :: [ VU.Vector (Int16, Int16) ]
bContours = [[(99,0),(99,328),(99,656),(192,656),(285,656),(335,656),(376,647),(418,638),(447,619),(477,600),(493,569),(510,539),(510,496),(510,447),(482,407),(454,368),(395,353),(395,351),(395,349),(467,338),(507,299),(547,260),(547,192),(547,144),(529,108),(511,72),(478,48),(446,24),(400,12),(355,0),(300,0),(199,0),(99,0)],[(182,380),(226,380),(271,380),(355,380),(391,407),(428,434),(428,489),(428,543),(389,564),(351,586),(275,586),(228,586),(182,586),(182,483),(182,380)],[(182,70),(235,70),(288,70),(373,70),(419,99),(465,129),(465,196),(465,257),(420,283),(375,310),(288,310),(235,310),(182,310),(182,190),(182,70)]]

rContours :: [ VU.Vector (Int16, Int16) ]
rContours = [[(100,0),(100,328),(100,656),(202,656),(304,656),(354,656),(396,646),(438,637),(468,615),(499,593),(516,558),(533,523),(533,472),(533,395),(493,349),(453,303),(386,286),(469,143),(553,0),(505,0),(458,0),(379,138),(300,277),(241,277),(183,277),(183,138),(183,0),(141,0),(100,0)],[(183,345),(237,345),(292,345),(369,345),(409,376),(450,408),(450,472),(450,537),(409,563),(369,589),(292,589),(237,589),(183,589),(183,467),(183,345)]]

rContoursSerif :: [ VU.Vector (Int16, Int16) ]
rContoursSerif = [[(1135,250),(1187,168),(1236,127),(1285,86),(1350,86),(1353,86),(1356,86),(1356,43),(1356,0),(1341,0),(1327,0),(1231,0),(1169,7),(1108,14),(1065,34),(1022,54),(991,90),(961,126),(926,184),(787,414),(649,645),(564,645),(479,645),(479,434),(479,223),(479,178),(492,150),(506,123),(529,109),(552,95),(582,90),(613,86),(647,86),(660,86),(674,86),(674,43),(674,0),(376,0),(78,0),(78,43),(78,86),(91,86),(104,86),(138,86),(168,90),(199,95),(222,109),(245,123),(258,150),(272,178),(272,223),(272,730),(272,1237),(272,1282),(258,1309),(245,1337),(222,1352),(199,1367),(168,1371),(138,1376),(104,1376),(91,1376),(78,1376),(78,1419),(78,1462),(353,1462),(629,1462),(895,1462),(1025,1363),(1155,1265),(1155,1067),(1155,985),(1128,923),(1102,862),(1059,817),(1016,772),(962,742),(908,712),(854,694),(994,472),(1135,250)],[(479,741),(551,741),(623,741),(713,741),(773,761),(833,781),(869,821),(905,861),(920,921),(936,981),(936,1061),(936,1143),(919,1200),(902,1258),(864,1294),(826,1331),(765,1347),(705,1364),(618,1364),(548,1364),(479,1364),(479,1052),(479,741)]]

quadContours :: [ VU.Vector (Int16, Int16) ]
quadContours = [VU.reverse [(100,0),(100,328),(100,656),(202,656),(304,656),(141,0),(100,0)]]

liContoursMono :: [ VU.Vector (Int16, Int16) ]
liContoursMono = [[(232,402),(232,287),(232,173),(260,181),(288,189),(317,197),(345,205),(349,171),(353,138),(276,116),(197,94),(118,72),(53,55),(44,89),(36,124),(91,136),(164,156),(164,279),(164,402),(111,402),(58,402),(58,435),(58,468),(111,468),(164,468),(164,578),(164,688),(105,688),(46,688),(46,721),(46,755),(191,755),(337,755),(337,721),(337,688),(284,688),(232,688),(232,578),(232,468),(277,468),(322,468),(322,435),(322,402),(277,402),(232,402)],[(929,297),(929,149),(929,2),(929,-25),(921,-39),(914,-53),(894,-61),(873,-68),(839,-70),(805,-71),(755,-71),(752,-57),(745,-42),(739,-26),(732,-13),(770,-14),(801,-15),(832,-15),(842,-14),(853,-14),(856,-11),(859,-7),(859,2),(859,117),(859,233),(747,233),(635,233),(619,200),(603,169),(587,138),(570,109),(652,119),(734,130),(725,147),(715,162),(706,178),(698,193),(721,201),(745,209),(761,184),(777,154),(794,125),(808,97),(822,70),(830,50),(805,39),(781,29),(777,40),(771,53),(765,67),(757,82),(680,71),(633,64),(586,57),(560,52),(535,48),(524,44),(513,41),(507,37),(504,48),(497,67),(490,87),(485,100),(497,103),(508,118),(519,133),(532,156),(538,165),(548,185),(558,206),(570,233),(511,233),(453,233),(453,78),(453,-76),(418,-76),(384,-76),(384,110),(384,297),(490,297),(596,297),(603,314),(609,332),(615,350),(621,367),(521,367),(421,367),(421,506),(421,646),(453,646),(486,646),(486,536),(486,427),(654,427),(822,427),(822,536),(822,646),(856,646),(890,646),(890,506),(890,367),(791,367),(692,367),(685,350),(678,332),(671,314),(663,297),(796,297),(929,297)],[(796,483),(778,466),(760,449),(743,463),(717,482),(692,502),(663,523),(598,472),(538,436),(532,444),(520,456),(509,469),(500,476),(530,493),(561,512),(592,531),(622,552),(597,571),(570,588),(544,606),(521,622),(538,637),(556,652),(580,636),(606,618),(633,600),(660,582),(683,600),(704,620),(725,640),(742,659),(766,649),(790,640),(771,618),(748,596),(726,575),(701,553),(730,534),(754,515),(778,497),(796,483)],[(694,745),(819,745),(945,745),(945,712),(945,680),(654,680),(364,680),(364,712),(364,745),(490,745),(617,745),(610,764),(601,785),(593,806),(584,822),(617,831),(650,840),(661,819),(673,793),(685,767),(694,745)]]

liContours :: [ VU.Vector (Int16, Int16) ]
liContours = [[(232,402),(232,287),(232,173),(260,181),(288,189),(317,197),(345,205),(349,171),(353,138),(276,116),(197,94),(118,72),(53,55),(44,89),(36,124),(91,136),(164,156),(164,279),(164,402),(111,402),(58,402),(58,435),(58,468),(111,468),(164,468),(164,578),(164,688),(105,688),(46,688),(46,721),(46,755),(191,755),(337,755),(337,721),(337,688),(284,688),(232,688),(232,578),(232,468),(277,468),(322,468),(322,435),(322,402),(277,402),(232,402)],[(929,297),(929,149),(929,2),(929,-25),(921,-39),(914,-53),(894,-61),(873,-68),(839,-70),(805,-71),(755,-71),(752,-57),(745,-42),(739,-26),(732,-13),(770,-14),(801,-15),(832,-15),(842,-14),(853,-14),(856,-11),(859,-7),(859,2),(859,117),(859,233),(747,233),(635,233),(619,200),(603,169),(587,138),(570,109),(652,119),(734,130),(725,147),(715,162),(706,178),(698,193),(721,201),(745,209),(761,184),(777,154),(794,125),(808,97),(822,70),(830,50),(805,39),(781,29),(777,40),(771,53),(765,67),(757,82),(680,71),(633,64),(586,57),(560,52),(535,48),(524,44),(513,41),(507,37),(504,48),(497,67),(490,87),(485,100),(497,103),(508,118),(519,133),(532,156),(538,165),(548,185),(558,206),(570,233),(511,233),(453,233),(453,78),(453,-76),(418,-76),(384,-76),(384,110),(384,297),(490,297),(596,297),(603,314),(609,332),(615,350),(621,367),(521,367),(421,367),(421,506),(421,646),(453,646),(486,646),(486,536),(486,427),(654,427),(822,427),(822,536),(822,646),(856,646),(890,646),(890,506),(890,367),(791,367),(692,367),(685,350),(678,332),(671,314),(663,297),(796,297),(929,297)],[(796,483),(778,466),(760,449),(743,463),(717,482),(692,502),(663,523),(598,472),(538,436),(532,444),(520,456),(509,469),(500,476),(530,493),(561,512),(592,531),(622,552),(597,571),(570,588),(544,606),(521,622),(538,637),(556,652),(580,636),(606,618),(633,600),(660,582),(683,600),(704,620),(725,640),(742,659),(766,649),(790,640),(771,618),(748,596),(726,575),(701,553),(730,534),(754,515),(778,497),(796,483)],[(694,745),(819,745),(945,745),(945,712),(945,680),(654,680),(364,680),(364,712),(364,745),(490,745),(617,745),(610,764),(601,785),(593,806),(584,822),(617,831),(650,840),(661,819),(673,793),(685,767),(694,745)]]

flatContours :: [ VU.Vector (Int16, Int16) ] -> [ Contour ] 
flatContours = concat . map (`flatClosedContour` [])
  where
    flatClosedContour :: VU.Vector (Int16, Int16) -> [ Contour ] -> [ Contour ]
    flatClosedContour ((< 3) . VU.length -> True) acc = reverse $ acc
    flatClosedContour rest acc = flatClosedContour (VU.drop 2 rest) ((toContour . VU.toList . VU.take 3 $ rest) : acc)
    toContour :: [ (Int16, Int16) ] -> Contour
    toContour [ p1, p2, p3 ] = Contour (uncurry V2 p1) (uncurry V2 p2) (uncurry V2 p3)
    toContour _ = error "invalid points number for contour"

extractYBoundings :: (Int16 -> Int16 -> Int16) -> [ Contour ] -> [ (V2 Int16) ]
extractYBoundings sm = map (\(i, (Contour (V2 _x0 y0) (V2 _x1 y1) (V2 _x2 y2))) -> V2 (sm (sm y0 y1) y2) (fromIntegral i)) . zip [ 0 .. ]

extractXBoundings :: (Int16 -> Int16 -> Int16) -> [ Contour ] -> [ (V2 Int16) ]
extractXBoundings sm = map (\(i, (Contour (V2 x0 _y0) (V2 x1 _y1) (V2 x2 _y2))) -> V2 (sm (sm x0 x1) x2) (fromIntegral i)) . zip [ 0 .. ]

makeBoundings :: ([ Contour ] -> [ (V2 Int16) ]) -> [ Contour ] -> VS.Vector (V2 Int16)
makeBoundings em =  VS.fromList . sortOn (\(V2 v _i) -> v) . em

makeYMaxBoundings :: [ Contour ] -> VS.Vector (V2 Int16)
makeYMaxBoundings = makeBoundings (extractYBoundings max)

makeYMinBoundings :: [ Contour ] -> VS.Vector (V2 Int16)
makeYMinBoundings = makeBoundings (extractYBoundings min)

makeXMaxBoundings :: [ Contour ] -> VS.Vector (V2 Int16)
makeXMaxBoundings = makeBoundings (extractXBoundings max)

makeXMinBoundings :: [ Contour ] -> VS.Vector (V2 Int16)
makeXMinBoundings = makeBoundings (extractXBoundings min)

makeBands :: VS.Vector Contour -> Int -> Int -> [ VS.Vector Contour ] 
makeBands contours unitsPerEm treshold = undefined

-- todo
-- add texture box
-- add texture for contours
loadContours :: (MonadIO m, MonadCleaner m) => Application -> Vma.Allocator -> PhysicalDevice -> Device -> V.Vector Word32 -> QueueResource -> CommandPoolResource -> ShaderResource -> PipelineResource -> m Present
loadContours Application { width, height } allocator phys device queueFamilyIndices queueRes@QueueResource {..} commandPoolRes@CommandPoolResource {..}  textureShaderRes pipelineRes = do
  -- liftIO . print . descriptorSetLayouts $ textureShaderRes
  textureDescriptorSetResource <- Lib.withDescriptorSetResource device (descriptorSetLayouts textureShaderRes) (descriptorSetLayoutCreateInfos textureShaderRes) acquireT
  let quadw = 1000 :: Float
  let quadh = 1000 :: Float
  let quadVertices = [ QuadVertex (V2 0 0) (V4 (0) 0 0 1)
                     , QuadVertex (V2 quadw 0) (V4 (0) 0 0  1)
                     , QuadVertex (V2 quadw quadh) (V4 (0) 0 0  1)
                     , QuadVertex (V2 0 quadh) (V4 (0) 0 0  1)
                     ] :: VS.Vector QuadVertex
  quadVerticesBuffer <- Lib.withVertexBuffer allocator quadVertices acquireT

  let texIndices = [ 0, 1, 2, 2, 3, 0 ] :: VS.Vector Word32
  indicesBuffer <- Lib.withIndexBuffer allocator texIndices acquireT

  let texUniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (fromIntegral width) (0) (fromIntegral height) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (1) (1) 0) (axisAngle (V3 0 0 1) ((0)/360*2*pi)) !*! (m33_to_m44 . scaled $ 1)
        }

  texBufferWriteDescriptorSet <- Lib.withUniformBufferDescriptorSet allocator queueFamilyIndices textureDescriptorSetResource 0 0 texUniform acquireT

  let textureFormat = FORMAT_R8G8B8A8_SRGB
  let pixels = renderDrawing 200 100 (PixelRGBA8 255 255 0 100) $ fill $ rectangle (V2 0 0) 200 100

  -- imageWriteDescriptorSet <- Lib.withCombinedImageSamplerDescriptorSet allocator phys device queueRes commandPoolRes textureDescriptorSetResource 2 1 textureFormat (fromIntegral . JP.imageWidth $ pixels) (fromIntegral . JP.imageHeight $ pixels) (imageData pixels) acquireT
  let contoursTexelFormat = FORMAT_R16G16_SINT
  -- let contours = VS.map (\(Contour (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) -> Contour (V2 y1 ( x1)) (V2 y2 (x2)) (V2 y3 x3)) (flatContours rContours) :: VS.Vector Contour
  --let contours = (flatContours rContoursSerif) :: VS.Vector Contour
  --let contours = flatContours quadContours :: VS.Vector Contour
  let contours = (flatContours liContours) :: [ Contour ]
  samplerBufferWriteDescriptorSet <- Lib.withSamplerBufferDescriptorSet allocator phys device queueRes commandPoolRes queueFamilyIndices textureDescriptorSetResource 0 1 contoursTexelFormat (VS.fromList contours) acquireT

  let contoursYMaxBoundings = makeYMaxBoundings contours
  contoursYMaxBoundingsSamplerBufferWriteDescriptorSet <- Lib.withSamplerBufferDescriptorSet allocator phys device queueRes commandPoolRes queueFamilyIndices textureDescriptorSetResource 0 2 contoursTexelFormat contoursYMaxBoundings acquireT

  let contoursYMinBoundings = makeYMinBoundings contours
  contoursYMinBoundingsSamplerBufferWriteDescriptorSet <- Lib.withSamplerBufferDescriptorSet allocator phys device queueRes commandPoolRes queueFamilyIndices textureDescriptorSetResource 0 3 contoursTexelFormat contoursYMinBoundings acquireT

  let contoursXMaxBoundings = makeXMaxBoundings contours
  contoursXMaxBoundingsSamplerBufferWriteDescriptorSet <- Lib.withSamplerBufferDescriptorSet allocator phys device queueRes commandPoolRes queueFamilyIndices textureDescriptorSetResource 0 4 contoursTexelFormat contoursXMaxBoundings acquireT

  let contoursXMinBoundings = makeXMinBoundings contours
  contoursXMinBoundingsSamplerBufferWriteDescriptorSet <- Lib.withSamplerBufferDescriptorSet allocator phys device queueRes commandPoolRes queueFamilyIndices textureDescriptorSetResource 0 5 contoursTexelFormat contoursXMinBoundings acquireT

  updateDescriptorSets device
    [ texBufferWriteDescriptorSet
  --  , imageWriteDescriptorSet
    , samplerBufferWriteDescriptorSet
    , contoursYMaxBoundingsSamplerBufferWriteDescriptorSet
    , contoursYMinBoundingsSamplerBufferWriteDescriptorSet
    , contoursXMaxBoundingsSamplerBufferWriteDescriptorSet
    , contoursXMinBoundingsSamplerBufferWriteDescriptorSet
    ] []

  pure Present
    { vertexBuffers = [ quadVerticesBuffer ]
    , indexBuffer = indicesBuffer
    , descriptorSets = descriptorSets (textureDescriptorSetResource :: DescriptorSetResource)
    , drawSize = fromIntegral . VS.length $ texIndices
    , pipelineResource = pipelineRes
    }

withShaderStages :: MonadIO m => Device -> m (ShaderResource, V.Vector ShaderModule)
withShaderStages device = do
  let vertCode = [vert|
  #version 460 core
  #extension GL_ARB_separate_shader_objects : enable

  layout(set = 2, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo;

  layout(location = 0) in vec2 inPosition;
  layout(location = 1) in vec4 inColor;

  layout(location = 0) out vec4 fragColor;

  void main() {
    gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
    fragColor = inColor;
  }

  |]
  let fragCode = [frag|
  #version 460 core

  #extension GL_ARB_separate_shader_objects : enable

  layout(location = 0) in vec4 fragColor;

  layout(location = 0) out vec4 outColor; 

  void main() {
    outColor = fragColor;
  }
  |]
  Lib.createShaderResource device [ vertCode, fragCode ]

withTextureShaderStages :: MonadIO m => Device -> m (ShaderResource, V.Vector ShaderModule)
withTextureShaderStages device = do
  let vertCode = [vert|
  #version 460
  #extension GL_ARB_separate_shader_objects : enable

  layout(set = 2, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo[4];

  layout(location = 0) in vec2 position;
  layout(location = 1) in vec4 color;
  layout(location = 2) in vec2 texCoord;

  layout(location = 0) out vec4 fragColor;
  layout(location = 1) out vec2 fragTexCoord;

  void main() {
    gl_Position = ubo[3].proj * ubo[3].view * ubo[3].model * vec4(position, 0.0, 1.0);
    fragColor = color;
    fragTexCoord = texCoord;
  }
  |]
  let fragCode = [frag|
  #version 460 core

  #extension GL_ARB_separate_shader_objects : enable

  layout(set = 2, binding = 1) uniform sampler2D texSampler;

  layout(location = 0) in vec4 fragColor;
  layout(location = 1) in vec2 fragTexCoord;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = texture(texSampler, fragTexCoord); // for image use texSampler, for shape created by rasterfic use fragColor
  }
  |]
  Lib.createShaderResource device [ vertCode, fragCode ]

withContoursShaderStages :: MonadIO m => Device -> m (ShaderResource, V.Vector ShaderModule)
withContoursShaderStages device = do
  let vertCode = [vert|
  #version 460 core
  #extension GL_ARB_separate_shader_objects : enable
  layout(set = 0, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo;

  layout(location = 0) in vec2 position;
  layout(location = 1) in vec4 color;

  layout(location = 0) out vec4 fragColor;

  void main() {
    gl_Position = ubo.proj * ubo.view * ubo.model * vec4(position, 0.0, 1.0);
    fragColor = color;
  }

  |]
  let fragCode = [frag|
  #version 460 core

  #extension GL_ARB_separate_shader_objects : enable

  layout(set = 0, binding = 1) uniform isamplerBuffer contours;
  layout(set = 0, binding = 2) uniform isamplerBuffer contoursYMaxBoundings;
  layout(set = 0, binding = 3) uniform isamplerBuffer contoursYMinBoundings;
  layout(set = 0, binding = 4) uniform isamplerBuffer contoursXMaxBoundings;
  layout(set = 0, binding = 5) uniform isamplerBuffer contoursXMinBoundings;

  layout(location = 0) in vec4 fragColor;

  layout(location = 0) out vec4 outColor;

  float countWindingNumberBezier2(vec2 p0, vec2 p1, vec2 p2);

  float countWindingNumberBezier2Axis(vec2 p0, vec2 p1, vec2 p2);
  /*
  C(t) = (p0 - 2p1 + p2)t^2 + 2(p1 - p0)t + p0
  let a = p0 - 2p1 + p2
  let b = p0 - p1
  let c = p0
  let d = sqrt(b^2 - ac)
  C(t) = at^2 - 2bt + c
  C'(t) = 2at - 2b
  A. a == 0
  t0 = t1 = c / 2b
  B. a != 0
  t0 = (b + d) / a
  t1 = (b - d) / a
  
  */
  const float unitsPerEm = 1000.0f;
  const float pixelsPerEm = 15.0f;

  void main() {
    //vec2 pos = gl_FragCoord.xy * vec2(1.0, -1.0) + vec2(0, 1080.0);
    vec2 pos = gl_FragCoord.xy * vec2(1.0, -1.0) + vec2(0, 800.0);

    /*if ( pos.x < 50
      && pos.x > 0
      && pos.y < 50
      && pos.y > 0 ) {
      outColor = vec4(1,1,0,1);
      return;
    }*/
    if ( pos.x < 750
      && pos.x > 700
      && pos.y < 350
      && pos.y > 300 ) {
      outColor = vec4(0.8,0,0,1);
      return;
    }
    int texSize = textureSize(contours);
    int yMaxSize = textureSize(contoursYMaxBoundings);
    int yMinSize = textureSize(contoursYMinBoundings);
    int xMaxSize = textureSize(contoursXMaxBoundings);
    int xMinSize = textureSize(contoursXMinBoundings);

    float windingNumber = 0.0f;
    for (int i = 0; i < texSize; i += 3) {
      vec2 p0 = texelFetch(contours, i).xy / unitsPerEm * pixelsPerEm;
      vec2 p1 = texelFetch(contours, i + 1).xy / unitsPerEm * pixelsPerEm;
      vec2 p2 = texelFetch(contours, i + 2).xy / unitsPerEm * pixelsPerEm;
      windingNumber += countWindingNumberBezier2(p0 - pos, p1 - pos, p2 - pos);
      //outColor = vec4(p1/255.0, 0, 1);
      //break;
    }
    float wn = clamp(windingNumber, 0.0f, 1.0f);
    //float wn = tanh(windingNumber * 2.0f);
    //outColor = 1.0f - vec4(wn * (1.0f - fragColor.xyz), 1.0f - (wn > 0.0f ? 1.0f : 0.0f) * fragColor.w);
    if (wn > 0.0f) {
      outColor = vec4(clamp(wn, fragColor.x, 1.0f) * fragColor.x, clamp(wn, fragColor.y, 1.0f) * fragColor.y, clamp(wn, fragColor.z, 1.0f) * fragColor.z, wn * fragColor.w);
    } else {
      outColor = vec4(yMaxSize + yMinSize + xMaxSize + xMinSize, 1.0f, 1.0f, 0.0f);
    }
    //outColor = vec4(1.0f - wn * (1.0f - fragColor.xyz), (wn > 0.0f ? 1.0f : 0.0f) * fragColor.w);
  }

  float countWindingNumberBezier2(vec2 p0, vec2 p1, vec2 p2) {
    int samplingNumber = 4;
    float d = 1.0f / float(samplingNumber + 1);
    float wny = 0.0f;
    float wnx = 0.0f;
    for (int i = 1; i <= samplingNumber; i += 1) {
      vec2 dy = vec2(0.0f, float(i) * d - 0.5f);
      wny += countWindingNumberBezier2Axis(p0 + dy, p1 + dy, p2 + dy);
      wnx -= countWindingNumberBezier2Axis(p0.yx + dy, p1.yx + dy, p2.yx + dy);
    }
    //wny += countWindingNumberBezier2Axis(p0, p1, p2);
    //wnx -= countWindingNumberBezier2Axis(p0.yx, p1.yx, p2.yx);
    /*float wnyr = -countWindingNumberBezier2Axis(p0 * vec2(-1,1), p1*vec2(-1,1), p2*vec2(-1,1));
    float wnxr = countWindingNumberBezier2Axis(p0.yx * vec2(1,-1), p1.yx* vec2(1,-1), p2.yx*vec2(1,-1));

    return (wny + wnx + wnyr + wnxr) / 4.0;
    */
    return (wny + wnx) / (2.0f * float(samplingNumber));
    //return wnx;
  }

  vec2 calcRoot(vec2 p0, vec2 p1, vec2 p2) {
    vec2 a = p0 - 2.0f * p1 + p2;
    vec2 b = p0 - p1;
    vec2 c = p0;
    float d = sqrt(max(pow(b.y, 2) - a.y * c.y, 0.0));
    if (abs(a.y) < 1e-5 * unitsPerEm) {
      if (abs(b.y) < 1e-5 * unitsPerEm) {
        return vec2(-1, -1);
      }
      float t0 = 0.5f * c.y / b.y;
      float t1 = t0;
      float xt0 = a.x * pow(t0, 2.0f) - 2.0f * b.x * t0 + c.x;
      float xt1 = a.x * pow(t1, 2.0f) - 2.0f * b.x * t1 + c.x;
      return vec2(xt0, xt1);
    } else {
      float t0 = (b.y + d) / a.y;
      float t1 = (b.y - d) / a.y;
      float xt0 = a.x * pow(t0, 2.0f) - 2.0f * b.x * t0 + c.x;
      float xt1 = a.x * pow(t1, 2.0f) - 2.0f * b.x * t1 + c.x;
      return vec2(xt0, xt1);
    }
  }

  float countWindingNumberBezier2Axis(vec2 p0, vec2 p1, vec2 p2) {
    uint p0p1p2 = (p0.y > 0 ? 0x8U : 0) | (p1.y > 0 ? 0x4U : 0) | (p2.y > 0 ? 0x2U : 0);
    uint tmp = 0x2e74U >> p0p1p2; // Font Rendering Directly from Glyph Outlines Eric Lengyel
    uint t0 = tmp & 0x1U;
    uint t1 = (tmp >> 1) & 0x1U;
    vec2 r0r1 = calcRoot(p0, p1, p2);
    float acc0 = t0 * - clamp(r0r1.x + 0.5, 0.0, 1.0);
    float acc1 = t1 * clamp(r0r1.y + 0.5, 0.0, 1.0);
    //float acc0 = t0 * (r0r1.x >= 0 ? - 1.0f : 0.0f);
    //float acc1 = t1 * (r0r1.y >= 0 ? 1.0f : 0.0f);
    return acc0 + acc1;
  }
  |]
  Lib.createShaderResource device [ vertCode, fragCode ]
