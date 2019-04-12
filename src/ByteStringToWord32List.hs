

import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32)
import Data.Bits (Bits, shiftL, (.|.))
import GHC.Int (Int64)


bs2Word32List :: BL.ByteString -> [Word32]
bs2Word32List byteString = map fromByteString (chunk 4 byteString) :: [Word32] where

  chunk :: Int64 -> BL.ByteString -> [BL.ByteString]
  chunk k = takeWhile (not . BL.null) . map (makeLittleEndian . (BL.take k)) . iterate (BL.drop k)
    where makeLittleEndian = BL.reverse

  fromByteString :: (Num a, Bits a) => BL.ByteString -> a
  fromByteString = BL.foldl go 0
    where go acc i = (acc  `shiftL` 8) .|. (fromIntegral i)

