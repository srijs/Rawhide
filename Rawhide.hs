import Data.Word (Word8)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Test.QuickCheck

{- Adler32 Rolling Hash Function -}

spec_a32_identity :: Int -> ByteString -> Word8 -> Property
spec_a32_identity m a w = (m > 0 && B.length a > 1) ==> a32init m (B.snoc (B.tail a) w) == a32roll m (B.length a) (a32init m a) (B.head a) w

a32init :: Int -> ByteString -> (Int, Int)
a32init m = B.foldl hash (1, 0)
  where hash (a, b) w = let a' = a + (fromIntegral w) in (a' `mod` m, (b + a') `mod` m)

a32roll :: Int -> Int -> (Int, Int) -> Word8 -> Word8 -> (Int, Int)
a32roll m z (a, b) wr wa = let a' = (a - wr' + wa') in (a' `mod` m, (b - (z * wr') + a' - 1) `mod` m)
  where wr' = fromIntegral wr
        wa' = fromIntegral wa

spec_a32_scan_identity :: Int -> ByteString -> Word8 -> Property
spec_a32_scan_identity m a w = (m > 0 && B.length a > 1) ==> a32scan m (B.length a) (B.snoc a w) == [h, h']
  where h  = a32init m a
        h' = a32roll m (B.length a) h (B.head a) w

a32scan :: Int -> Int -> ByteString -> [(Int, Int)]
a32scan m z s = scan (a32init m (B.take z s)) s
  where scan h s = if B.length s >= z
                 then let Just (wr, s') = B.uncons s in h : scan (a32roll m z h wr (B.index s z)) s'
                 else []

chunk :: Int -> Int -> (Int, Int) -> ByteString -> [ByteString]
chunk m z (a, b) s =  

{- Arbitrary instances (for testing purposed) -}

instance Arbitrary B.ByteString where
    arbitrary   = fmap B.pack arbitrary
