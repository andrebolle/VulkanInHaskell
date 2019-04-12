{-# LANGUAGE DuplicateRecordFields #-}

module Pipeline where

import Control.Applicative (many)

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word( Word64
                , Word32
                )

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)

import Graphics.Vulkan

import System.IO
import System.IO.Error

readShader :: VkDevice -> String -> IO VkShaderModule
readShader device fileName = do
  input <- BL.readFile fileName
  let spirvCode = runGet (many getWord32le) input
  allocaArray (length spirvCode) $ \spirvCodePtr -> do
   pokeArray spirvCodePtr spirvCode
   with (createShaderModule (fromIntegral (length spirvCode)) spirvCodePtr) $ \createInfoPtr -> do
     alloca $ \shaderModulePtr -> do
       vkCreateShaderModule device createInfoPtr nullPtr shaderModulePtr
       peek shaderModulePtr

createShaderModule bytesCount wordsPtr = VkShaderModuleCreateInfo
  { vkSType       = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO   -- :: VkStructureType
  , vkPNext       = nullPtr                                       -- :: Ptr Void
  , vkFlags       = VkShaderModuleCreateFlags 0                   -- :: VkShaderModuleCreateFlags
  , vkCodeSize    = bytesCount                                    -- :: CSize
  , vkPCode       = wordsPtr                                      -- :: Ptr Word32
  }

  -- tryIOError :: IO (Either IOError Handle)
  -- okOrNot <- tryIOError (openBinaryFile fileName ReadMode)
  -- bytes <- case okOrNot of
  --           -- ioError :: IOError -> IO a
  --           Right file ->
  --             do  -- BL.hGetContents :: GHC.IO.Handle.Types.Handle -> IO BL.ByteString
  --                bytes <- hGetContents file
  --                -- putStrLn $ "Length is: " ++ show (length bytes)
  --                --let word32s = fromString bytes
  --                putStrLn "shaders/vert.spv found"
  --                -- return bytes
  --           Left _notOK ->  error $ fileName ++ " not there"
