{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE KindSignatures #-}

-- stack build
-- stack ghci src/Main.hs

--stack exec vulkan-example
--stack exec vulkan-example|grep ERROR

-- stack ghci src/Main.hs --ghci-options -fobject-code
-- TODO: Need to add validation layers to aid debugging

module Main where



import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Graphics.Vulkan
import Graphics.Vulkan.Version

import qualified Graphics.Vulkan as PDP (VkPhysicalDeviceProperties(vkApiVersion, vkVendorID))
import qualified Graphics.Vulkan as QFP (VkQueueFamilyProperties(vkQueueFlags, vkQueueCount, vkTimestampValidBits, vkMinImageTransferGranularity))
import qualified Graphics.Vulkan as E3D (VkExtent3D(vkWidth, vkHeight, vkDepth))

import Foreign.Storable (peek, poke)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (with)
import Foreign.C.String (newCString, withCString, CString)
import Foreign.C.Types

import Data.Vector.Storable.Sized hiding ((++))
import GHC.TypeLits
--import Data.Vector.Storable.Sized (Vector(..)) -- hiding ((++))

import Data.Bits ((.&.), shift)

import Data.Char (intToDigit)
import Numeric (showIntAtBase)

--import Data.Word( Word64
--                , Word32
--                )
-- FYI: type VkFlags = Word32

import GHC.Word

import Text.Groom

import qualified Graphics.UI.GLFW as GLFW

import InstanceAndPhysicalDevice (createVkInstance, findPhysicalDevice)
import LogicalDeviceAndQueue (createLogicalDeviceAndQueue)
--import RenderPass (renderPass)
import RenderPassAndDescriptorSet
import Pipeline

import BinaryTest

-- newtype NewTestThing v (n :: Nat) a = NewTestThing (v a)
--newtype NewTestThing v a = NewTestThing (v a) deriving (Show)

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    --, envGear1         :: !GL.DisplayList
    --, envGear2         :: !GL.DisplayList
    --, envGear3         :: !GL.DisplayList
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    }

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show


main :: IO ()
main = do

    let width  = 640
        height = 480

    -- Create a quete of window events
    eventsChan <- newTQueueIO :: IO (TQueue Event)

    vkInstance <- createVkInstance
    vkPhysicalDevice <- findPhysicalDevice vkInstance

    getPhysicalDeviceMemoryProperties vkPhysicalDevice

    printAPIVersion vkPhysicalDevice

    (device, queue) <- createLogicalDeviceAndQueue vkPhysicalDevice
    renderPass <- createRenderPass device

    vertShader <- readShader device "shaders/vert.spv"
    fragShader <- readShader device "shaders/frag.spv"

    putStrLn "Yey!"

    -- withWindow width height "Vulkan Demo" doGraphics

    vkDeviceWaitIdle device
    vkDestroyRenderPass device renderPass nullPtr
    vkDestroyDevice device nullPtr
    vkDestroyInstance vkInstance nullPtr

-- ************************************* sandpit **************************************************
-- prettyprinter

-- <quchen> purelazy: I’ve got a significant overhaul of the wl-pprint lib in the pipeline with lots of
-- documentation, so if you’re looking for examples run »stack haddock --open wl-pprint« in this repo:
-- https://github.com/quchen/prettyprinter/

getPhysicalDeviceMemoryProperties vkPhysicalDevice =
  with deviceMemoryProperties $ \vkPhysicalDeviceMemoryPropertiesPtr -> do
    vkGetPhysicalDeviceMemoryProperties vkPhysicalDevice vkPhysicalDeviceMemoryPropertiesPtr
    properties <- peek vkPhysicalDeviceMemoryPropertiesPtr
    putStrLn $ "vkMemoryTypeCount: " ++ show (vkMemoryTypeCount properties)
    putStrLn $ "properties: " ++ (groom properties)
    return ()

memoryTypes = Data.Vector.Storable.Sized.replicate VkMemoryType { vkPropertyFlags = VkMemoryPropertyFlagBits 0, vkHeapIndex = 0  } :: Vector VK_MAX_MEMORY_TYPES VkMemoryType
memoryHeaps = Data.Vector.Storable.Sized.replicate VkMemoryHeap { vkSize = VkDeviceSize 0, vkFlags = VkMemoryHeapFlagBits 0 } :: Vector VK_MAX_MEMORY_HEAPS VkMemoryHeap

deviceMemoryProperties =
  VkPhysicalDeviceMemoryProperties{ vkMemoryTypeCount = 0
                                  , vkMemoryTypes = memoryTypes
                                  , vkMemoryHeapCount = 0
                                  , vkMemoryHeaps = memoryHeaps
                                  }

-- ************************************* doGraphics **************************************************
doGraphics :: GLFW.Window -> IO ()
doGraphics win = do
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  setupCallbacks win eventsChan

  GLFW.swapInterval 1

  -- here

  mainLoop win

  return ()

-- ************************************* mainloop **************************************************
mainLoop :: GLFW.Window -> IO ()
mainLoop win = do
  GLFW.pollEvents
  shouldClose <- GLFW.windowShouldClose win
  if (shouldClose) then return () else do
    -- drawframe
    mainLoop win

-- ************************************* Native Window **************************************************
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    -- Set error callback
    GLFW.setErrorCallback $ Just simpleErrorCallback
    -- Init
    r <- GLFW.init -- r is GLFW_TRUE if successful, or GLFW_FALSE if an error occurred.
    when r $ do
        -- Creates a new window. Note: If running in GHCI don't forget to
        -- `:set -fno-ghci-sandbox` or you may run into an assertion failure, segfault or other nasty crash.
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win -- run the graphics code
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

-- *************************************  GLFW Callback Code **************************************************
setupCallbacks :: GLFW.Window -> TQueue Event -> IO ()
setupCallbacks win eventsChan = do
  GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan  -- report errors
  GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan  -- window mpved
  GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan  -- .. resized
  GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan  -- .. closed
  GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan  -- when the contents of a window is damaged and needs to be refreshed
  GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan  -- when a window gains or loses input focus
  GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan  --  when a window is iconified or restored
  GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan  --  the framebuffer of a window is resized
  GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan  -- mouse click
  GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan  -- cursor position
  GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan  -- cursor enter exit window
  GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan  -- scrolling
  GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan  --  a physical key is pressed or released or when it repeats
  GLFW.setCharCallback            win $ Just $ charCallback            eventsChan  -- regular text input
-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

-- Notes

-- Ignore stuff from here - these are my notes.

-- ************************************* glfwCreateWindowSurface **************************************************

-- GLFW 3.2 Support #69
-- https://github.com/bsl/GLFW-b/issues/69

-- Hmmm GLFW 3.1 - does Vulkan require 3.2?!

--The GLFW-b package
--[Tags:bsd3, library, test]
--Bindings to GLFW (http://www.glfw.org/), an open source, multi-platform library for creating windows with OpenGL contexts and managing input and events.

--GLFW-b depends on bindings-GLFW (http://hackage.haskell.org/package/bindings-GLFW), which, as of the time of this writing,
--binds to GLFW 3.1, released 2015-01-18 (http://www.glfw.org/Version-3.1-released.html http://www.glfw.org/changelog.html).


--class vk2Surface {
--private:
--    VkSurfaceKHR surface;
--public:
--  vk2Surface(VkInstance instance, GLFWwindow* window) {
--    if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
--            throw std::runtime_error("failed to create window surface!");
--  }

--  VkSurfaceKHR getSurface() { return surface; }

--};

-- *************************************  Vulkan Validation Layer **************************************************

-- /home/andre2/Desktop/clones/vulkan-examples/src/Main.hs:185:15: error:
--    Data constructor not in scope:
--      VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
--        :: VkStructureType

        --VkDebugReportCallbackCreateInfoEXT createInfo = {};
        --createInfo.sType = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT;
        --createInfo.flags = VK_DEBUG_REPORT_ERROR_BIT_EXT | VK_DEBUG_REPORT_WARNING_BIT_EXT;
        --createInfo.pfnCallback = debugCallback;

--In vulkan.h

--    typedef struct VkDebugReportCallbackCreateInfoEXT {
--        VkStructureType                 sType;
--        const void*                     pNext;
--        VkDebugReportFlagsEXT           flags;
--        PFN_vkDebugReportCallbackEXT    pfnCallback;
--        void*                           pUserData;
--    } VkDebugReportCallbackCreateInfoEXT;

--mkVkDebugReportCallbackCreateInfoEXT debugCallbackPtr = VkDebugReportCallbackCreateInfoEXT
--  { vkSType = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
--  , vkPNext = nullPtr
--  , vkFlags = VK_DEBUG_REPORT_ERROR_BIT_EXT .|. VK_DEBUG_REPORT_WARNING_BIT_EXT
--  , vkPfnCallback = debugCallbackPtr
--  , vkPUserData = nullPtr
--  }


--Buffer swapping

--    GLFW windows are by default double buffered. That means that you have two rendering buffers;
--    a display (aka front) buffer and a render (aka back) buffer. The display buffer is the one being displayed and the render
--    buffer the one being rendered.

--    When the entire frame has been rendered, it is time to switch buffers
--    in order to display what has been rendered and begin rendering a new frame. This is done with glfwSwapBuffers.

--    glfwSwapBuffers(window);
--    Sometimes it can be useful to select when the buffer swap will occur. With the function
--    glfwSwapInterval it is possible to select the minimum number of monitor refreshes the
--    driver wait should from the time glfwSwapBuffers was called before swapping the buffers:

--    glfwSwapInterval(N);
--    If the interval is zero, the swap will take place immediately when glfwSwapBuffers is
--    called without waiting for a refresh. Otherwise at least N refreshes will pass between each buffer
--    swap. Using a swap interval of zero can be useful for benchmarking purposes, when it is not desirable to
--    measure the time it takes to wait for the vertical retrace. However, a swap interval of one lets you avoid tearing.

--    Note that this may not work on all machines, as some drivers have user-controlled settings
--    that override any swap interval the application requests.

--import Data.Bits

--bitwise :: Int -> Int -> IO ()
--bitwise a b = do
--  print $ a .&. b
--  print $ a .|. b
--  print $ a `xor` b
--  print $ complement a
--  print $ shiftL a b -- left shift
--  print $ shiftR a b -- arithmetic right shift
--  print $ shift a b  -- You can also use the "unified" shift function; positive is for left shift, negative is for right shift
--  print $ shift a (-b)
--  print $ rotateL a b -- rotate left
--  print $ rotateR a b -- rotate right
--  print $ rotate a b  -- You can also use the "unified" rotate function; positive is for left rotate, negative is for right rotate
--  print $ rotate a (-b)


-- Hi all,
--
-- I'm looking at some code that defines a record:
--
--    data Position =
--           Position { posOffset :: {-# UNPACK #-} !Int
--                    , posRow :: {-# UNPACK #-} !Int
--                    , posColumn :: {-# UNPACK #-} !Int
--                    }
--
-- What does the the exclamation mark mean? And UNPACK?

--The ! means "strict" – i.e. don't store a thunk for an Int here, but
--evaluate it first.

--The {-# UNPACK #-} tells the compiler that it can unpack the Int –
--meaning that a Position will be neatly packed into 12 bytes.


-- Experimenting with alloca, bit-wise ops
printAPIVersion vkPhysicalDevice =
  alloca $ \propertiesPtr -> do
    vkGetPhysicalDeviceProperties vkPhysicalDevice propertiesPtr
    properties <- peek propertiesPtr
   --  print $ fromIntegral $ vkApiVersion (properties :: VkPhysicalDeviceProperties)
    let version = (fromIntegral $ PDP.vkApiVersion properties)::Int

    --The major version number is a 10-bit integer packed into bits 31-22.
    --The minor version number is a 10-bit integer packed into bits 21-12.
    --The patch version number is a 12-bit integer packed into bits 11-0.

    let major = shift version (negate 22)
    let minor = shift version (negate 12) .&. 0x3ff
    let patch = version .&. 0xfff
    -- Vulkan API Version: 1.0.37 at the time of writing
    -- TODO - I seem to get 1.0.8!!
    putStrLn $ "Version: " ++ show major ++ "." ++ show minor ++ "." ++ show patch
    putStrLn $ showIntAtBase 2 intToDigit version ""

toHex n = showIntAtBase 16 intToDigit n ""
