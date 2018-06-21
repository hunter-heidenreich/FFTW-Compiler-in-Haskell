-- Simple function definition
doubleMe x = x + x

-- Two input function
doubleUs x y = doubleMe x + doubleMe y

-- If example, expanded
doubleSmallNumber x = if x > 100
  then x
  else x * 2

-- One line if example
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- Example of a definition (name) and using ' in a declaration
conanO'Brien = "It's a-me, Conan O'Brien!"

-- A basic list
theList = [1, 2, 3, 4, 5]

-- Appending lists
appendLists l1 l2 = l1 ++ l2

-- Cons Example
zeroFront l = 0:l

-- Access example
getFirst l = l !! 0

-- Basic ranges with step size
listByThrees from to = [from, (from + 3)..to]

-- Using comprehensions
doubleList from to = [2*x | x <- [from..to]]

-- Conditional comprehensions
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- Multpile lists
allProducts l1 l2 = [x*y | x <- l1, y <- l2]

-- New length
length' xs = sum [1 | _ <- xs]

-- Tuple example
numberElements xs = zip [1..] xs

-- Right triangle Example
triangles from to = [ (a,b,c) | c <- [from..to], b <- [from..to], a <- [from..to] ]
rightTriangles from to = [ (a,b,c) | c <- [from..to], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' from to = [ (a,b,c) | c <- [from..to], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

-- Illustrating type declaration
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Factorial
--factorial :: Integer -> Integer
--factorial lim = product [1..lim]

-- Example of pattern matching in functions
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- Recursive factorial definition
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Pattern matching with lists
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- Recursive length
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

-- Guards example
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
          -- Alternate: (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Combining list comprehensions and Guards
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let Example
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- Let binding example
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- Case example
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- Recursively defined max
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
-- Alternate recursive call: maximum' (x:xs) = max x (maximum' xs)

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
