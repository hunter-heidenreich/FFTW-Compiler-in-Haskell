# Typeclasses

- Typeclasses are similar to Java interfaces
- They are used in type definitions
  - Ex:
- Though they are used in class constraints, what they do is guarantee certain
  behavior
  - Ex: Eq gives (==) and (/=)
  ```
  show the Eq typeclass, a new data type, and adding it to Eq
  ```

# Functors
 - Functors are a typeclass
 - Containers of data
 - Guarantee data type can be mapped over
  - Underlying data can be modified without caring about container
  - Ex: lists, Maybes, functions
 - Gives fmap (also represented as <$>):
 ```
 class Functor f where
  fmap :: (a -> b) -> f a -> f b
 ```
 - Example:
 ```
 Show some examples in action
  instance Functor [] where
    fmap = map

  instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

  instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

 ```
 - Laws:
  - Mapping the id function over a functor should return the functor unchanged
  - Composing two maps should yield the same result as mapping one function and
    then mapping the other.

# Applicative Functors
 - Beefier functors
 - A way to combine wrapped data
 - Allow for functions in a context
  - Ex: functions in a list, functions in maybes, etc
 - The typeclass:
 ```
 class (Functor f) => Applicative f
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b
 ```
  - pure takes a value and wraps it in the minimal context
  - sequential application takes a wrapped function and applies it to a wrapped
    value
 - Example:
 ```
 (4 *) <$> (Just 5) -> pure (4 *) <*> (Just 5) == Just 20
 (4 *) <$> Nothing -> pure (4 *) <*> Nothing == Nothing
 pure (*) <*> (Just 4) <*> (Just 5) == Just 20
 pure (*) <*> Nothing <*> (Just 5) == Nothing

 instance Applicative Maybe where
  pure = Just
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just x) = Just (f x)

  instance Applicative [] where
    pure a = [a]
    fs <*> xs = [f x | f <- fs, x <- xs]

  pure (4 *) <*> [1,2,3] == [4,8,12]
  [(1+), (5*), (10*)] <*> [1,2,3] == [2,3,4,5,10,15,10,20,30]
  pure (*) <*> [1,2,3] <*> [10,20,30] == [10,20,30,20,40,60,30,60,90]
 ```
 - Laws:
  - Wrapping the identity function and applying it to a value should return the
    value
  - Homomorphism: wrapping pure to a function and a value and applying them
    should yield the same result as applying a function to a value and then
    wrapping a pure
  - Function composition should hold

# Monads
 - Even beefier than applicative functors
 - Not explicitly applicative functors, but mathematically they are
 - Wraps a value/computation in a context
  - Provides a way to wrap a value (return)
  - Provides a way to combine computations in a context (bind or >>=)
    - Bind operator allows for values in a context to be passed to functions
      that should take a plain value and return it in a context
- The typeclass:
  ```
  class Monad m where
    return :: a -> m a
    (>>=) :: m a -> a -> m b -> m b
  ```

- The laws:
  - Return should only wrap a function (identity)


## Maybe Monad
- Motivation for monads: the triangle pattern
 ```
  maybeFunc1 :: String -> Maybe Int
  maybeFunc1 “” = Nothing
  maybeFunc1 str = Just $ length str

  maybeFunc2 :: Int -> Maybe Float
  maybeFunc2 i = if i `mod` 2 == 0
    then Nothing
    else Just ((fromIntegral i) * 3.14159)

  maybeFunc3 :: Float -> Maybe [Int]
  maybeFunc3 f = if f > 15.0
    then Nothing
    else $ Just [floor f, ceil f]

  runMaybeFuncs :: String -> Maybe [Int]
  runMaybeFuncs input = case maybeFunc1 input of
    Nothing -> Nothing
    Just i -> case maybeFunc2 i of
      Nothing -> Nothing
      Just f -> maybeFunc3 f

  instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    Just a >>= f = f a

  runMaybeFuncs :: String -> Maybe [Int]
  runMaybeFuncs input = maybeFunc1 input >>= maybeFunc2 >>= maybeFunc3

  runMaybeFuncs :: String -> Maybe [Int]
  runMaybeFuncs input = do
    i <- maybeFunc1 input
    f <- maybeFunc2 f
    maybeFunc3 f
```
- Maybe preserves failure, this allows for if a function fails, the whole thing
  fails
- <- unwraps values from monads in do notation

## Either Monad
- Monad for viewing either a value, or a failure with information
  ```
  instance Monad (Either a) where
    return r = Right r
    (Left l) >>= _ = Left l
    (Right r) >>= f = f r
  ```
- Example:
  ```
  maybeFunc1 :: String -> Either String Int
  maybeFunc1 “” = Left “String cannot be empty!”
  maybeFunc1 str = Right $ length str

  maybeFunc2 :: Int -> Either String Float
  maybeFunc2 i = if i `mod` 2 == 0
    then Left “Length cannot be even!”
    else Right ((fromIntegral i) * 3.14159)

  maybeFunc3 :: Float -> Either String [Int]
  maybeFunc3 f = if f > 15.0
    then Left “Float is too large!”
    else $ Right [floor f, ceil f]

  runMaybeFuncs :: String -> Either String [Int]
  runMaybeFuncs input = do
    i <- maybeFunc1 input
    f <- maybeFunc2 i
    maybeFunc3 f
  ```
## IO Monad
  ```
  main :: IO ()
  main = do
    -- getLine :: IO String
    input <- getLine
    let uppercased = map Data.Char.toUpper input
    -- print :: String -> IO ()
    print uppercased
  ```

## Global Variables with Monads

### Reader
 - Motivation for globals:
  ```
  main :: IO ()
  main = do
    env <- loadEnv
    let str = func1 env
    print str

  data Environment = Environment
    { param1 :: String
    , param2 :: String
    , param3 :: String }

  loadEnv :: IO Environment
  loadEnv = …

  func1 :: Environment -> String
  func1 env = “Result: “ ++ (show (func2 env))

  func2 :: Environment -> Int
  func2 env = 2 + floor (func3 env)

  func3 :: Environment -> Float
  func3 env = … -- Some calculation based on the environment
  ```

- The Reader Monad
    ```
    main :: IO ()
    main = do
    env <- loadEnv
    let str = runReader func1 env
    print str

    data Environment = Environment
    { param1 :: String
    , param2 :: String
    , param3 :: String }

    loadEnv :: IO Environment
    loadEnv = …

    func1 :: Reader Environment String
    func1 = do
    res <- func2
    return (“Result: “ ++ (show res))

    func2 :: Reader Environment Int
    func2 = do
    env <- ask
    let res3 = func3 env
    return (2 + (floor res3))

    func3 :: Environment -> Float
    ...
    ```

- runReader creates a context in which the values supplied act like global variables
- ask gives value

### Writer
  - Motivation
    ```
    -- Calls func2 if even length, func3 and func4 if odd
    func1 :: String -> (Int, String)
    func1 input = if length input `mod` 2 == 0
      then func2 (0, input)
      else (i1 + i2, str1 ++ str2)
        where
          (i1, str1) = func3 (0, tail input)
          (i2, str2) = func4 (0, take 1 input)

    -- Calls func4 on truncated version
    func2 :: (Int, String) -> (Int, String)
    func2 (prev, input) = if (length input) > 10
      then func4 (prev + 1, take 9 input)
      else (10, input)

    -- Calls func2 on expanded version if a multiple of 3
    func3 :: (Int, String) -> (Int, String)
    func3 (prev, input) = if (length input) `mod` 3 == 0
      then (prev + f2resI + 3, f2resStr)
      else (prev + 1, tail input)
      where
        (f2resI, f2resStr) = func2 (prev, input ++ "ab")

    func4 :: (Int, String) -> (Int, String)
    func4 (prev, input) = if (length input) < 10
      then (prev + length input, input ++ input)
      else (prev + 5, take 5 input)
  ```

  - Monoids
    ```
    class Monoid a where
      mempty :: a
      mappend :: a -> a -> a

    instance Monoid Int where
      memty = 0
      mappend a b = a + b
    ```

  - The Writer Monad
    ```
    func1 :: String -> (String, Int)
    func1 input = if length input `mod` 2 == 0
      then runWriter (func2 input)
      else runWriter $ do
        str1 <- func3 input
        str2 <- func4 (take 1 input)
        return (str1 ++ str2)

    func2 :: String -> Writer Int String
    func2 input = if (length input) > 10
      then do
        tell 1
        func4 (take 9 input)
      else do
        tell 10
        return input

    func3 :: String -> Writer Int String
    func3 input = if (length input) `mod` 3 == 0
      then do
        tell 3
        func2 (input ++ “ab”)
      else do
        tell 1
        return $ tail input

    func4 :: String -> Writer Int String
    func4 input = if (length input) < 10
      then do
        tell (length input)
        return (input ++ input)
      else do
        tell 5
        return (take 5 input)
    ```

    - tell uses monoid to apply value

### State
  - Reading and writing
  - get = ask
  - put = tell
