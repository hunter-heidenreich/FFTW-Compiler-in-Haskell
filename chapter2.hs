-- Arithmetic example
arith3 x y z = ((x + y + z) - (2 * y)) / (-7)

-- Boolean example
implication x y = not x || y

-- Equality example. Also /= for inequality
bothOdd x y = (odd x) == (odd y)

-- Previous functions were prefix
-- Operators were infix

-- Example of defining an infix function
x `div2` y = x / y

-- No capital letters to start functions, but ' is valid
nothing' = "This does nothing" -- Is also a declaration

-- LIST SUB-SECTION

-- List definitions for examples
giveIntList = [1, 9, 4, 2, 5]
giveCharList = ['f', 'E', 'O', 'p'] -- Is a string

-- Append lists
append xs ys = xs ++ ys

-- Cons examples
tackZero xs = 0:xs

-- Get element example
getEl xs i = xs !! i

-- Arbitrary list comparison
forwardGTEReverse xs = xs >= (reverse xs)

-- Range examples
getRange from to = [from..to]
getInfiniteRange from = [from..]
getRangeWithStep from step to = [from,(from+step)..to]

-- List comprehensions examples
doubleList from to = [2*x | x <- [from..to]]                              -- basic
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]     -- conditional
allProducts xs ys = [x*y | x <- xs, y <- ys]                              -- multiple lists
boundedEvenRange from to = [x | x <- [from..to], x > 0, x < 1001, even x] -- multiple conditions

-- TUPLE SUB-SECTION

-- Basic example
-- *note* fst and snd only work with pairs
addTup2 tup1 tup2 = ((fst tup1) + (fst tup2), (snd tup1) + (snd tup2))

-- Zip example
theZip xs ys = zip xs ys
