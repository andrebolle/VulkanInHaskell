{-# LANGUAGE DuplicateRecordFields #-}

module LogicalDeviceAndQueue (createLogicalDeviceAndQueue) where

import Data.Bits ((.&.))

import Foreign.C.String (newCString, withCString, CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)

import GHC.Word
import Graphics.Vulkan


createLogicalDeviceAndQueue :: VkPhysicalDevice -> IO (VkDevice, VkQueue)
createLogicalDeviceAndQueue vkPhysicalDevice =

  -- Allocate a pointer to the "family count"
  alloca $ \familyCountPtr -> do

    -- Get the "family count"
    vkGetPhysicalDeviceQueueFamilyProperties vkPhysicalDevice familyCountPtr nullPtr
    queueFamilyCount <- peek familyCountPtr

    -- Allocate an array for "family properties"
    allocaArray (fromIntegral queueFamilyCount) $ \vkQueueFamilyPropertiesPtr -> do
        -- Fill the family properties array
        vkGetPhysicalDeviceQueueFamilyProperties vkPhysicalDevice familyCountPtr vkQueueFamilyPropertiesPtr
        -- Read array into a list of VkQueueFamilyProperties records
        queueFamilyPropertiesList <- peekArray  (fromIntegral queueFamilyCount) vkQueueFamilyPropertiesPtr

        -- Create a list of vkQueueFlags only which makes finding a graphics queue index easier.
        let queueFlagsList = queueFlagsOnly queueFamilyPropertiesList

        -- Find the index of the first element with the graphic bit set
        let graphicsQueueIndex = findGraphicsQueueFamilyIndex queueFlagsList

        putStrLn $ "Graphics queue family index is: " ++ show graphicsQueueIndex

        -- Create device and (graphics) queue
        alloca $ \priorityPtr -> do
          poke priorityPtr 1.0
          let queueCreateInfo = mkVkDeviceQueueCreateInfo priorityPtr
          allocaArray 1 $ \queueCreateArrayPtr -> do

            pokeArray queueCreateArrayPtr [queueCreateInfo]
            with (mkVkDeviceCreateInfo queueCreateArrayPtr) $ \deviceCreateInfoPtr ->
              alloca $ \logicalDevicePtr -> do
                putStrLn "Calling vkCreateDevice"

                vkCreateDevice vkPhysicalDevice deviceCreateInfoPtr nullPtr logicalDevicePtr
                device <- peek logicalDevicePtr

                alloca $ \queuePtr -> do
                  putStrLn "Calling vkGetDeviceQueue"

                  -- TODO: I think vkGetDeviceQueue causes a segmentation fault
                  vkGetDeviceQueue device graphicsQueueIndex 0 queuePtr
                  
                  putStrLn "Finished vkGetDeviceQueue"
                  queue <- peek queuePtr
                  return (device, queue)
  where

    -- Build the logical device creation record
    mkVkDeviceCreateInfo queueCreateInfoPtr = VkDeviceCreateInfo 
      { vkSType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
      , vkPNext = nullPtr
      , vkFlags = VkDeviceCreateFlags 0
      , vkQueueCreateInfoCount = 1
      , vkPQueueCreateInfos = queueCreateInfoPtr
      , vkEnabledLayerCount = 0
      , vkPpEnabledLayerNames = nullPtr
      , vkEnabledExtensionCount = 0
      , vkPpEnabledExtensionNames = nullPtr
      , vkPEnabledFeatures = nullPtr
      }

    mkVkDeviceQueueCreateInfo priorityPtr = VkDeviceQueueCreateInfo
      { vkSType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
      , vkPNext = nullPtr
      , vkFlags = VkDeviceQueueCreateFlags 0
      , vkQueueFamilyIndex = 0
      , vkQueueCount = 1
      , vkPQueuePriorities = priorityPtr
      }

-- Create a list of vkQueueFlags only which makes finding a graphics queue index easier.
queueFlagsOnly :: [VkQueueFamilyProperties] -> [GHC.Word.Word32]
queueFlagsOnly queueFamilyPropertiesRecords = map getBits queueFamilyPropertiesRecords where
    getBits :: VkQueueFamilyProperties -> GHC.Word.Word32
    getBits queueFamilyPropertiesRecord = let   VkQueueFlagBits bits = vkQueueFlags queueFamilyPropertiesRecord in bits

-- Return the first position in the list where the graphics bit is set.
findGraphicsQueueFamilyIndex :: [GHC.Word.Word32] -> GHC.Word.Word32
findGraphicsQueueFamilyIndex list = whereInListN list 0 where
    whereInListN :: [GHC.Word.Word32] -> GHC.Word.Word32 -> GHC.Word.Word32
    whereInListN [] _ = error "Could not find a match"
    whereInListN (word:words) n = let  graphics_bit = 1 in if (word .&. graphics_bit /= 0) then n else whereInListN words (n + 1) 