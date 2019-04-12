{-# LANGUAGE DuplicateRecordFields #-}

module InstanceAndPhysicalDevice (createVkInstance, findPhysicalDevice) where

import Data.Bits (zeroBits)
import GHC.Word

import Foreign.C.String (newCString, withCString, CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import Graphics.Vulkan



-- FYI newCString :: String -> IO CString
-- type CString = Ptr CChar   -- Defined in ‘Foreign.C.String’
-- newtype CChar = CChar GHC.Int.Int8   -- Defined in ‘Foreign.C.Types’

createVkInstance :: IO VkInstance
createVkInstance =
  withCString "AppName" $ \namePtr ->
      with (mkVkApplicationInfo namePtr) $ \appInfo -> do
          layerNamesPtr <- allocValidationLayerArray validationLayerStrings
          with (mkVkInstanceCreateInfo appInfo layerNamesPtr) $ \instInfo ->
              alloca $ \instPtr -> do
                  vkResult <- vkCreateInstance instInfo nullPtr instPtr
                  print vkResult
                  peek instPtr
  where

    --validationLayerStrings = ["VK_LAYER_LUNARG_standard_validation", "VK_LAYER_LUNARG_monitor", "VK_LAYER_LUNARG_api_dump"]
    validationLayerStrings = ["VK_LAYER_LUNARG_standard_validation"]

    allocValidationLayerArray :: [String] -> IO (Ptr CString)
    allocValidationLayerArray validationLayerStrings =
      allocaArray (length validationLayerStrings) $ \array -> do
        arrayOfStringPtrs <- Prelude.mapM newCString validationLayerStrings
        putStrLn $ show arrayOfStringPtrs
        pokeArray array arrayOfStringPtrs
        return array

    mkVkApplicationInfo namePtr = VkApplicationInfo
        { vkSType               = VK_STRUCTURE_TYPE_APPLICATION_INFO
        , vkPNext               = nullPtr
        , vkPApplicationName    = namePtr
        , vkApplicationVersion  = 1
        , vkPEngineName         = namePtr
        , vkEngineVersion       = 0
        , vkApiVersion          = vkMakeVersion 1 0 37
        }

    mkVkInstanceCreateInfo appInfo layerNamesPtr = VkInstanceCreateInfo
        { vkSType                     = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
        , vkPNext                     = nullPtr -- castPtr debugInfo
        , vkFlags                     = VkInstanceCreateFlags zeroBits
        , vkPApplicationInfo          = appInfo
        , vkEnabledLayerCount         = fromIntegral $ length validationLayerStrings
        , vkPpEnabledLayerNames       = layerNamesPtr
        , vkEnabledExtensionCount     = 0
        , vkPpEnabledExtensionNames   = nullPtr
        }

-- Find a physical device
findPhysicalDevice :: VkInstance -> IO VkPhysicalDevice
findPhysicalDevice vkInstance =
  alloca $ \integerPtr -> do
    vkResult <- vkEnumeratePhysicalDevices vkInstance integerPtr nullPtr
    gpuCount <- peek integerPtr
    allocaArray (fromIntegral gpuCount) $
      \arrayPtr -> do
        vkEnumeratePhysicalDevices vkInstance integerPtr arrayPtr
        peek arrayPtr
