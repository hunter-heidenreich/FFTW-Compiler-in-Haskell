-- Recursively defined max
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
-- Alternate recursive call: maximum' (x:xs) = max x (maximum' xs)

-- Recursive min
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum of empty list"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

-- Recursion and guards
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

-- Take recursive definition
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- Recursive length
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

-- Recursive reverse (using slow append)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

-- Recursive drop
drop' :: Int -> [a] -> [a]
drop' n xs
  | n <= 0 = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

-- Recursive sum
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + (sum' xs)

-- Recursive product
product' :: (Num a) => [a] -> a
product' [] = 0
product' [x] = x
product' (x:xs) = x * (product' xs)

-- Recusive elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' i (x:xs)
  | i /= x = elem' i xs
  | i == x = True
