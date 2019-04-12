module BinaryTest where 

-- Data
import Control.Applicative (many)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word (Word32)

binaryTest :: IO [Word32]
binaryTest = do
  input <- BL.readFile "shaders/vert.spv"
  return $ runGet (many getWord32le) input