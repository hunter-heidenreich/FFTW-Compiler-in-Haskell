-- Currying functions --

multByThree :: (Num a) => a -> a
multByThree = (*) 3

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10/)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Higher Order Functions --
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyThrice :: (a -> a) -> a -> a
applyThrice f x = f (f (f x))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- Maps and filters --
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

squareList :: (Num a) => [a] -> [a]
squareList xs = map' (^2) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

oddSquaresSum :: (Integral a) => a -> a
oddSquaresSum cap = sum (takeWhile (<cap) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

maxLongChain :: Int -> Int
maxLongChain cap = maximum (map length (map chain [1..cap]))

infMults :: (Num a, Enum a) => [a -> a]
infMults = map (*) [1..]

-- Lambdas --
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

mysteryMath :: (Fractional a) => [a] -> [a] -> [a]
mysteryMath xs ys = zipWith (\a b -> (a * 30 + 3) / b) xs ys

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- Folds and scans --
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

minimum' :: (Ord a) => [a] -> a
minimum' = foldl1 (\acc x -> if x < acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- Application with $ --
sumRoots :: (Floating a, Enum a) => a -> a -> a
sumRoots from to = sum $ map sqrt [from..to]

operations :: (Floating a) => a -> [a]
operations x = map ($ x) [(4+), (10*), (^2), sqrt]

-- Function composition
negateAbs :: (Num a) => [a] -> [a]
negateAbs xs = map (negate . abs) xs

mathfn :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
mathfn = ceiling . negate . tan . cos . max 50
-- mathfn x = ceiling(negate (tan (cos (max 50 x))))

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
