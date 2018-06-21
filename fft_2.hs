-- ASSUMPTION: lg(n) \in {natural numbers}
import qualified Data.List as List
import Data.Complex
import qualified Data.Vector as V

type CVec = V.Vector (Complex Double)

ones_vec :: Int -> CVec
ones_vec n = V.replicate n (1:+0)

stepVec :: Int -> CVec
stepVec n = V.enumFromN (1:+0) n

l :: CVec -> CVec
l x = V.fromList $ [x V.! (2*i) | i <- [0..n-1]] ++ [x V.! (2*i+1) | i <- [0..n-1]]
        where n = div (V.length x) 2

i2f :: CVec -> CVec
i2f x = (fft n $ V.fromList [x V.! i | i <- [0..n-1]]) V.++ (fft n $ V.fromList [x V.! i | i <- [n..2*n-1]])
          where n = div (V.length x) 2

t2 :: CVec -> CVec
t2 x = V.fromList $ [x V.! i | i <- [0..n-1]] ++ [(x V.! (i + n)) * ((exp (0:+(pi/fromIntegral(n)))) ^ i) | i <- [0..n-1]]
        where n = div (V.length x) 2

f2i :: CVec -> CVec
f2i x = V.fromList $ [(x V.! i) + (x V.! (i + n)) | i <- [0..n-1]] ++ [(x V.! i) - (x V.! (i + n)) | i <- [0..n-1]]
        where n = div (V.length x) 2

fft :: Int -> CVec -> CVec
fft 2 x = V.fromList [(x V.! 0) + (x V.! 1), (x V.! 0) - (x V.! 1)]
fft _ x = (f2i . t2 . i2f . l) x

dft_check_sum :: Int -> CVec -> CVec
dft_check_sum n x = V.fromList $ [(sum [(x V.! j) * (exp ((0:+(2*pi*fromIntegral(j)*fromIntegral(k))) / fromIntegral(n))) | j <- [0..(n-1)]]) | k <- [0..(n-1)]]

diff_vec :: CVec -> CVec -> Complex Double
diff_vec v1 v2 = sum $ [abs $ (v1 V.! i) - (v2 V.! i) | i <- [0..(V.length v1) - 1] ]
