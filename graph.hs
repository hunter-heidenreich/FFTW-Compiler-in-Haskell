import Data.Complex

data Node = Numb (Complex Double) | Load String | Plus [Node] | Times [Node] | UMinus Node

instance Show Node where
  show (Numb x) = "(" ++ show x ++ ")"
  show (Load x) = "(" ++ show x ++ ")"
  show (Plus x) = "(+ " ++ unwords ([show n | n <- x]) ++ ")"
  show (Times x) = "(* " ++ unwords ([show n | n <- x]) ++ ")"
  show (UMinus x) = "(- " ++ show x ++ ")"

fftgen :: Int -> (Int -> Node) -> (Int -> Node)
fftgen 1 input = input
fftgen n input = cooley_tukey n (div n 2) 2 input

cooley_tukey :: Int -> Int -> Int -> (Int -> Node) -> (Int -> Node)
cooley_tukey n n1 n2 x =
  let inner j2 = fftgen n1 (\j1 -> x (n2 * j1 + j2)) in
    let twiddle i1 j2 = Times [(omega n ((-1) * j2 * i1)), (inner j2 i1)] in
      let outer i1 = fftgen n1 (twiddle i1) in
        (\i -> outer (mod i n2) (div i n2))

omega :: Int -> Int -> Node
omega n k = Numb (exp (0:+(2 * pi * fromIntegral(k))/fromIntegral(n)))

get_load :: Int -> Node
get_load i = Load ("X" ++ show i)

{-|cooley_tukey n p q x sign =
  let inner j2 = fftgen q (\j1 -> x (p * j1 + j2)) sign in
    let twiddle k1 j2 = (omega n (j2 * k1)) * (inner j2 k1) in
      let outer k1 = fftgen p (twiddle k1) sign in
        (\k -> outer (k mod q) (k / q))|-}

{-|cooley_tukey n1 n2 input sign =
  let tmp1 j2 = fftgen n1 (\j1 -> input (j1 * n2 + j2)) sign in
    let tmp2 i1 j2 = (exp ((0:+(2 * pi * sign * i1 * j2)/fromIntegral(n1 * n2)))) * (tmp1 j2 i1) in
      let tmp3 i1 = fftgen n2 (tmp2 i1) sign in
        (\i -> tmp3 (i mod n1) (i / n1))|-}
