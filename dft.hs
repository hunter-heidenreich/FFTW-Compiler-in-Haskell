import qualified Data.List as List
import Data.Complex

type Matrix a = [[a]]

-- Things to Do:
  -- Tensor Add
  -- W_s (i) matrix
  -- T^{RS}_S matrix
  -- Matrix multiplication


-- Definitions
f2 :: (Num a) => Matrix a
f2 = [[1,1],[1,-1]]

omegaSubN n = 1 / n

-- Matrix types
zeros :: (Num a) => Int -> Int -> Matrix a
zeros rows cols = [[0 | j <- [0..(cols - 1)]] | i <- [0..(rows - 1)]]

ones :: (Num a) => Int -> Int -> Matrix a
ones rows cols = [[1 | j <- [0..(cols - 1)]] | i <- [0..(rows - 1)]]

identity :: (Num a) => Int -> Matrix a
identity dim = [[if (i == j) then 1 else 0 | j <- [0..(dim-1)]] | i <- [0..(dim-1)]]

diagonal :: (Num a) => [a] -> Matrix a
diagonal diag = [[if (i == j) then diag !! j else 0 | j <- [0..(dim-1)]] | i <- [0..(dim-1)]]
                where dim = length diag

permutationMatrix :: (Num a) => Int -> Int -> Matrix a
permutationMatrix size stride = [ident !! x | x <- List.sortBy (\a b -> compare (rem a stride) (rem b stride)) [0..(size - 1)]]
                                  where ident = identity size


transpose :: (Num a) => Matrix a -> Matrix a
transpose mat = List.transpose mat

scale :: (Num a, Eq a, Ord a) => a -> Matrix a -> Matrix a
scale s mat
      | s == 0    = zeros (length mat) (length $ mat !! 0)
      | s == 1    = mat
      | s < 0     = scale (abs s) $ List.map (List.map (*(-1))) mat
      | otherwise = List.map (List.map (*s)) mat

getRow :: (Num a) => Matrix a -> Int -> Matrix a
getRow mat row = [mat !! row]

getCol :: (Num a) => Matrix a -> Int -> Matrix a
getCol mat col = [[row !! col] | row <- mat]

hBlock :: (Num a) => Matrix a -> Matrix a -> Matrix a
--hBlock mat1 mat2 = [r1 ++ r2 | r1 <- mat1, r2 <- mat2]
hBlock mat1 mat2 = [(mat1 !! i) ++ (mat2 !! i) | i <- [0..((length mat1) - 1)]]

vBlock :: (Num a) => Matrix a -> Matrix a -> Matrix a
vBlock mat1 mat2 = mat1 ++ mat2

tensorMult mat1 mat2 = foldl1 vBlock [List.foldl1 hBlock x | x <- blockMat]
                        where blockMat = [[scale e mat2 | e <- r] | r <- mat1]

{-|
--zeroMatrix :: Size -> Matrix
--zeroMatrix (w, h) = [[0 | i <- [0..w-1]] | j <- [0..h-1]]

oneMatrix :: Size -> Matrix
oneMatrix (w, h) = [[1 | i <- [0..w-1]] | j <- [0..h-1]]

specialMatrix :: Size -> Matrix
specialMatrix (w, h) = [[(1.0 * i+  1.0 * j) | i <- [0..w-1]] | j <- [0..h-1]]

identityMatrix :: Size -> Matrix
identityMatrix (w, h) = [[if i==j then 1 else 0| i <- [0..w-1]] | j <- [0..h-1]]

--permutationMatrix (w, h) stride = [[1 | i <- stride, ] j <-[]
--permutationMatrix :: (Int, Int) -> Int -> Matrix
--permutationMatrix (w, h) stride = [ (identityMatrix (w, h)) !! ((rem j stride) * stride) + (floor $ (fromIntegral j) / (fromIntegral stride)) | j <- [0..h-1]]
|-}
