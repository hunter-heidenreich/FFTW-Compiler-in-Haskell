class  Eq a  where
  (==), (/=)  ::  a -> a -> Bool

  x /= y  = not (x == y)
  x == y  = not (x /= y)


class  (Eq a) => Ord a  where
  compare              :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a

  compare x y | x == y    = EQ
              | x <= y    = LT
              | otherwise = GT

  x <= y  = compare x y /= GT
  x <  y  = compare x y == LT
  x >= y  = compare x y /= LT
  x >  y  = compare x y == GT

  -- Note that (min x y, max x y) = (x,y) or (y,x)
  max x y | x <= y    =  y
          | otherwise =  x
  min x y | x <= y    =  x
          | otherwise =  y


class  Enum a  where
  succ, pred     :: a -> a
  toEnum         :: Int -> a
  fromEnum       :: a -> Int
  enumFrom       :: a -> [a]            -- [n..]
  enumFromThen   :: a -> a -> [a]       -- [n,n'..]
  enumFromTo     :: a -> a -> [a]       -- [n..m]
  enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]


class  Functor f  where
  fmap    :: (a -> b) -> f a -> f b


class  Monad m  where
  (>>=)   :: m a -> (a -> m b) -> m b
  (>>)    :: m a -> m b -> m b
  return  :: a -> m a
  fail    :: String -> m a

  m >> k  =  m >>= \_ -> k
  fail s  = error s
