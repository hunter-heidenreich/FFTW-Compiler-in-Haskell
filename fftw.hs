import Data.Complex

type Label = Int
data ExprL = AddL Label ExprL ExprL | MinusL Label ExprL | MultL Label ExprL ExprL | ConstantL Label (Complex Double) | VariableL Label String deriving Show

type ExprM = State Label ExprL

new_labelM :: State Label Label
new_labelM = do
  p <- get
  put (p+1)
  return p

run_exprM :: ExprM -> ExprL
run_exprM m = evalState m 0

instance Eq ExprL where
  eq == e2 = label e1 == label e2
  where label (AddL p _ _) = p
        label (MinusL p _) = p
        label (MultL p _ _) = p
        label (ConstantL p _) = p
        label (VariableL p _) = p


fftgen :: Int -> (Int -> Node (Complex Double)) -> Int -> (Int -> Node (Complex Double))
fftgen 2 input sign = (\i -> if (even i)  then Plus [input i, input (i + 1)]
                                          else Plus [input (i - 1), UMinus (input i)])
fftgen n input sign = cooley_tukey (div n 2) 2 input sign

load_input :: Int -> Node (Complex Double)
load_input i = Load (fromIntegral(i) :: (Complex Double))

output :: Int -> Node (Complex Double)
output i = (fftgen 8 (load_input) (1)) i

output_n :: Int -> Int -> Node (Complex Double)
output_n n i = (fftgen n (load_input) (1)) i

cooley_tukey :: Int -> Int -> (Int -> Node (Complex Double)) -> Int -> (Int -> Node (Complex Double))
cooley_tukey n1 n2 input sign =
  let inner j2 = fftgen n1 (\j1 -> input (j1 * n2 + j2)) sign in
    let twiddle i1 j2 = Times (omega (n1 * n2) (sign * i1 * j2), inner j2 i1) in
      let outer i1 = fftgen n2 (twiddle i1) sign in
        (\i -> outer (mod i n1) (div i n1))


omega :: Int -> Int -> Node (Complex Double)
omega n k = Number (exp (0:+(2 * pi * fromIntegral(k))/fromIntegral(n)))
