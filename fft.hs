-- ASSUMPTION: lg(n) \in {natural numbers}
import qualified Data.List as List
import Data.Complex
import qualified Data.Vector as V

type CVec = V.Vector (Complex Double)

ones_vec :: Int -> CVec
ones_vec n = V.replicate n (1:+0)

stepVec :: Int -> CVec
stepVec n = V.enumFromN (1:+0) n

l :: Int -> CVec -> CVec
l n2 x = V.fromList $ [x V.! (2*i) | i <- [0..n-1]] ++ [x V.! (2*i+1) | i <- [0..n-1]]
        where n = div n2 2

i2f :: Int -> CVec -> CVec
i2f n2 x = (fft n $ V.fromList [x V.! i | i <- [0..n-1]]) V.++ (fft n $ V.fromList [x V.! i | i <- [n..n2-1]])
        where n = div n2 2

t2 :: Int -> CVec -> CVec
t2 n2 x = V.fromList $ [x V.! i | i <- [0..n-1]] ++ [(x V.! (i + n)) * ((exp (0:+(pi/fromIntegral(n)))) ^ i) | i <- [0..n-1]]
        where n = div n2 2

f2i :: Int -> CVec -> CVec
f2i n2 x = V.fromList $ [(x V.! i) + (x V.! (i + n)) | i <- [0..n-1]] ++ [(x V.! i) - (x V.! (i + n)) | i <- [0..n-1]]
        where n = div n2 2

fft :: Int -> CVec -> CVec
fft 2 x = V.fromList [(x V.! 0) + (x V.! 1), (x V.! 0) - (x V.! 1)]
fft n2 x = f2i n2 $ t2 n2 $ i2f n2 $ l n2 x

dft_check_sum :: Int -> CVec -> CVec
dft_check_sum n x = V.fromList $ [(sum [(x V.! j) * (exp ((0:+(2*pi*fromIntegral(j)*fromIntegral(k))) / fromIntegral(n))) | j <- [0..(n-1)]]) | k <- [0..(n-1)]]

diff_vec :: CVec -> CVec -> Complex Double
diff_vec v1 v2 = sum $ [abs $ (v1 V.! i) - (v2 V.! i) | i <- [0..(V.length v1) - 1] ]

{-|
l_mat :: Int -> CVec -> CVec
l_mat n x = [x !! (2*i) | i <- [0..n-1]] ++ [x !! (2*i+1) | i <- [0..n-1]]

i2f :: Int -> CVec -> CVec
i2f n x = fft n [x !! i | i <- [0..n-1]] ++ fft n [x !! i | i <- [n..2*n-1]]

t2 :: Int -> CVec -> CVec
t2 n x = [x !! i | i <- [0..2*n-1]] ++ [(x !! i) * (exp (0:+(pi*fromIntegral(i))/fromIntegral(n))) | i <- [0..2*n-1]]

f2i :: Int -> CVec -> CVec
f2i n x = [(x !! i) + (x !! (i + n)) | i <- [0..n-1]] ++ [(x !! i) - (x !! (i + n)) | i <- [n..2*n-1]]

fft :: Int -> CVec -> CVec
fft 2 x = [(x !! 0) + (x !! 1), (x !! 0) - (x !! 1)]
fft n x = f2i n (t2 n (i2f n (l_mat n x)))

dft_check_sum :: Int -> CVec -> CVec
dft_check_sum n x = [(sum [(x !! j) * (exp ((0:+(2*pi*fromIntegral(j)*fromIntegral(k))) / fromIntegral(n))) | j <- [0..(n-1)]]) | k <- [0..(n-1)]]
 |-}
