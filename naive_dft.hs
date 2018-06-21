-- ASSUMPTION: lg(n) \in {natural numbers}
import qualified Data.List as List
import Data.Complex

type Vector = [Complex Double]

x_vec :: Int -> Vector
x_vec n = [fromIntegral(i):+0 | i <-[0..(n-1)]]

slice :: Int -> Int -> Vector -> Vector
slice from to xs = take (to - from + 1) (drop from xs)

halfVec :: Int -> Vector -> (Vector, Vector)
halfVec n x = let n_half = div n 2
              in ((slice 0 (n_half - 1) x), (slice n_half n x))

l_mat :: Int -> Vector -> Vector
l_mat n x = [x !! i | i <- List.sortBy (\a b -> compare (rem a 2) (rem b 2)) [0..(n - 1)]]

i_tensor_f :: Int -> Vector -> Vector
i_tensor_f n x = let n_half = div n 2
                     splitVec = halfVec n x
                     x' = fst splitVec
                     x'' = snd splitVec
                 in (dft_mat n_half x') ++ (dft_mat n_half x'')

t_mat :: Int -> Vector -> Vector
t_mat n x = let n_half = div n 2
                splitVec = halfVec n x
                x'  = fst splitVec
                x'' = snd splitVec
                omegaX = [(x'' !! i) * ((exp ((0:+2 * pi)/ fromIntegral(n))) ^ i) | i <- [0..n_half-1]]
            in x' ++ omegaX


f_tensor_i :: Int -> Vector -> Vector
f_tensor_i n x = let n_half = div n 2
                  in [(x !! i) + (x !! (i+n_half)) | i <- [0..n_half-1]] ++ [(x !! i) - (x !! (i+n_half)) | i <- [0..n_half-1]]

dft_mat :: Int -> Vector -> Vector
dft_mat 2 x = [(x !! 0) + (x !! 1), (x !! 0) - (x !! 1)]
dft_mat n x = f_tensor_i n (t_mat n (i_tensor_f n (l_mat n x)))

dft_check_sum :: Int -> Vector -> Vector
dft_check_sum n x = [(sum [(x !! j) * (exp ((0:+(2*pi*fromIntegral(j)*fromIntegral(k))) / fromIntegral(n))) | j <- [0..(n-1)]]) | k <- [0..(n-1)]]
