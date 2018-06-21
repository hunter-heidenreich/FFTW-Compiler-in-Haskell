
type Matrix = String

-- F_RS = (F_R X I_S) T^(RS)_S (I_R X F_S) L^(RS)_R
-- ASSUMPTION: POW OF 2
-- F_2N = (F_2 X I_N) T^(2N)_N (I_2 X F_N) L^(2N)_2

dft_matrix :: Int -> Matrix
dft_matrix 2 = "DFT_2"
dft_matrix size = tensor_mult (dft_matrix 2) (identity n) ++ (sub_matrix size n) ++ tensor_mult (identity 2) (dft_matrix n) ++ (permutation size 2)
                  where n = div size 2

identity :: Int -> Matrix
identity size = "I_" ++ (show size)

permutation :: Int -> Int -> Matrix
permutation size stride = "L_(" ++ (show size) ++ ".." ++ (show stride) ++ ")"

sub_matrix :: Int -> Int -> Matrix
sub_matrix rs s = "T^(" ++ (show rs) ++ ")_" ++ (show s)

tensor_mult :: Matrix -> Matrix -> Matrix
tensor_mult mat1 mat2 = "(" ++ mat1 ++ " X " ++ mat2 ++ ")"
