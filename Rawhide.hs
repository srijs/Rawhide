import Data.Word (Word8)
import Data.Int (Int64)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Test.QuickCheck

{- Adler32 Rolling Hash Function -}

spec_a32_identity :: Int -> ByteString -> Word8 -> Property
spec_a32_identity m a w = (m > 0 && B.length a > 1) ==> a32init m (B.snoc (B.tail a) w) == a32roll m (B.length a) (a32init m a) (B.head a) w

a32init :: Int -> ByteString -> (Int, Int)
a32init m = B.foldl hash (1, 0)
  where hash (a, b) w = let a' = a + (fromIntegral w) in (a' `mod` m, (b + a') `mod` m)

a32roll :: Int -> Int64 -> (Int, Int) -> Word8 -> Word8 -> (Int, Int)
a32roll m z (a, b) wr wa = let a' = (a - wr' + wa') in (a' `mod` m, (b - ((fromIntegral z) * wr') + a' - 1) `mod` m)
  where wr' = fromIntegral wr
        wa' = fromIntegral wa

spec_a32_scan_identity :: Int -> ByteString -> Word8 -> Property
spec_a32_scan_identity m a w = (m > 0 && B.length a > 1) ==> a32scan m (B.length a) (B.snoc a w) == [h, h']
  where h  = a32init m a
        h' = a32roll m (B.length a) h (B.head a) w

a32scan :: Int -> Int64 -> ByteString -> [(Int, Int)]
a32scan m z s = scan (a32init m (B.take z s)) s
  where scan h s = let (w, r) = B.splitAt z s in
                   if B.length w == z
                   then let Just (wr, s') = B.uncons s in h : scan (a32roll m z h wr (B.index s z)) s'
                   else []

{- Chunking -}

group :: (Eq a) => Int64 -> Int64 -> a -> [a] -> [Int64]
group min max c cs = groupRun c cs 0
  where groupRun _ []     n = [n]
        groupRun c (h:hs) n = if h == c && n >= min || n == max
                              then n : groupRun c hs 1
                              else groupRun c hs (n + 1)

chunk :: Int64 -> Int64 -> Int -> Int64 -> (Int, Int) -> ByteString -> [ByteString]
chunk min max m z c cs = chunkRun (group min max c (a32scan m z cs)) cs
  where chunkRun [_]    cs = [cs]
        chunkRun (l:ls) cs = let (c, cs') = B.splitAt l cs in c : chunkRun ls cs'

{- Arbitrary instances (for testing purposed) -}

instance Arbitrary ByteString where
    arbitrary = fmap B.pack arbitrary
