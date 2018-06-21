import Data.Complex

data Node a = Number (Complex a) | Load a | Plus [Node a] | Times [Node a] | UMinus (Node a)

instance (Show a) => Show (Node a) where
  show (Number x) = "(" ++ show x ++ ")"
  show (Load x) = "(" ++ show x ++ ")"
  show (Plus x) = "(+ " ++ unwords ([show n | n <- x]) ++ ")"
  show (Times x) = "(* " ++ unwords ([show n | n <- x]) ++ ")"
  show (UMinus x) = "(- " ++ show x ++ ")"

fftgen :: Int -> (Int -> Node Double) -> Int -> (Int -> Node Double)
fftgen 2 input sign = (\i -> if (even i)  then Plus [input i, input (i + 1)]
                                          else Plus [input (i - 1), UMinus (input i)])
fftgen n input sign = cooley_tukey (div n 2) 2 input sign

load_input :: Int -> Node Double
load_input i = Load (fromIntegral(i) :: Double)

output i = (fftgen 8 (load_input) (1)) i

cooley_tukey n1 n2 input sign =
  let inner j2 = fftgen n1 (\j1 -> input (j1 * n2 + j2)) sign in
    let twiddle i1 j2 = Times [omega (n1 * n2) (sign * i1 * j2), inner j2 i1] in
      let outer i1 = fftgen n2 (twiddle i1) sign in
        (\i -> outer (mod i n1) (div i n1))


omega :: Int -> Int -> Node Double
omega n k = Number (exp (0:+(2 * pi * fromIntegral(k))/fromIntegral(n)))
