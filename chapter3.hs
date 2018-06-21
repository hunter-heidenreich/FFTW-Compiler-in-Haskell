-- Type inference
addSeven x = x + 7

-- Illustrating type declaration
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Factorial
factorial :: Integer -> Integer
factorial lim = product [1..lim]

-- type declaration with multiple variables
appendChar :: Char -> String -> String
appendChar c st = c:st

-- Common Types
{-|
   Int     – a bounded integer (has a min and max value)
  	Integer – an unbounded integer
  	Float   – real floating point with single precision
  	Double  – real floating point with double precision
  	Bool    – True or False
  	Char    – a character
  	String  – list of characters
  	Tuples  - their own type (based on size)
|-}

-- Type classes - similar to Java interfaces but better
{-|
•	Eq – supports equality checking (== and /=)
•	Ord – Supports comparison (must also belong to Eq)
•	Show – Supports conversion to string (basically all types except functions)
•	Read – Converts strings to literal values
•	Enum – members are sequentially ordered (they can have ranges and can use the succ and pred functions)
•	Bounded – have upper and lower bounds
•	Num – has the property of being a number
  o	Whole numbers are polymorphic constants, until otherwise defined
  o	Must also belong to Show and Eq
•	Integral – a Num sub-class that includes only whole numbers
  o	fromIntegral – a useful function for converting to floats from ints
•	Floating – a Num sub-class that includes only floating-point numbers
|-}
