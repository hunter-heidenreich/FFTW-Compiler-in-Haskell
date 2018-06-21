import Data.Complex
import Data.Hashable
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.List as L

-- DATA DEFS
type ExprMap = M.Map Int Node
type DependencyMap = M.Map Int [Node]

init_dag :: [Node] -> Dag
init_dag ns = Dag {
                  nodeMap = M.empty,
                  cMap = M.empty,
                  nodes = ns}

data Dag = Dag {cMap :: DependencyMap, nodeMap :: ExprMap, nodes :: [Node]} deriving (Show)
data Node = Number (Complex Double) | Load Var | Plus [Node] | Times (Node, Node) | UMinus (Node) | Store (Var, Node)
data Var = Temp Int | Inp Int | Const Int | Out Int | None

instance Show Node where
  show (Number x) = show x
  show (Load x) = "(Load" ++ show x ++ ")"
  show (Plus x) = "(+ " ++ unwords ([show n | n <- x]) ++ ")"
  show (Times (x, y)) = "(* " ++ show x ++ " " ++ show y ++ ")"
  show (UMinus x) = "-" ++ show x
  --show (Store (x, y)) = "(Store" ++ show x ++ ", " ++ show y ++ ")"
  show (Store (x, y)) = "(Store" ++ show x ++ ")"

instance Eq Node where
  (Number x) == (Number y) = x == y
  (Plus xs) == (Plus ys) = (subsetOf xs ys) && (subsetOf ys xs)
  (Times (a, b)) == (Times (c, d)) = ((a == c) && (b == d)) || ((a == d) && (b == c))
  (UMinus a) == (UMinus b) = a == b
  (Load a) == (Load b) = a == b
  _ == _ = False

subsetOf :: (Eq a) => [a] -> [a] -> Bool
subsetOf xs ys = null $ filter (not . (`elem` ys)) xs

instance Eq Var where
  (Temp x) == (Temp y) = x == y
  (Inp x) == (Inp y) = x == y
  (Out x) == (Out y) = x == y
  (Const x) == (Const y) = x == y
  _ == _ = False

instance Show Var where
  show (Temp x) = "(temp" ++ show x ++ ")"
  show (Inp x) = "(x[" ++ show x ++ "])"
  show (Out x) = "(y[" ++ show x ++ "])"
  show (Const x) = "(" ++ show x ++ "])"
  show None = "NONE"
-- END DATA DEFS

-- HASHING FUNCTIONS
hash_expr :: Node -> Int
hash_expr (Number x) = (hash (realPart x)) + 16719 *(hash (imagPart x)) + 7
hash_expr (Plus xs) = 5 + 23451 * sum (map hash_expr xs)
hash_expr (Times (x, y)) = 41 + 31415 * (hash_expr x +  hash_expr y)
hash_expr (UMinus x) = 42 + 12345 * (hash_expr x)
hash_expr (Load v) = 1 + 1237 * hash_var v
hash_expr (Store (v, e)) = 2 * hash_var v - 2345 * hash_expr e

hash_var :: Var -> Int
hash_var (Temp x) = hash x
hash_var (Inp x) = hash x
hash_var (Out x) = hash x
hash_var (Const x) = hash x
hash_var None = hash "None"
-- END HASHING FUNCTIONS

-- GENERATION
fftgen :: Int -> (Int -> Node) -> Int -> (Int -> Node)
fftgen 2 input sign = (\i -> if (even i)  then Plus [input i, input (i + 1)]
                                          else Plus [input (i - 1), UMinus (input i)])
fftgen n input sign = cooley_tukey 2 (div n 2) input sign

load_input :: Int -> Node
load_input i = Load (Inp i)

output :: Int -> Node
output i = (fftgen 8 (load_input) (1)) i

output_n :: Int -> Int -> Node
output_n n i = Store (Out i, (fftgen n (load_input) (1)) i)

outputs_n :: Int -> [Node]
outputs_n n = [output_n n i| i <- [0..n-1]]

cooley_tukey :: Int -> Int -> (Int -> Node) -> Int -> (Int -> Node)
cooley_tukey n1 n2 input sign =
  let inner j2 = fftgen n1 (\j1 -> input (j1 * n2 + j2)) sign in
    let twiddle i1 j2 = Times (omega (n1 * n2) (sign * i1 * j2), inner j2 i1) in
      let outer i1 = fftgen n2 (twiddle i1) sign in
        (\i -> outer (mod i n1) (div i n1))


omega :: Int -> Int -> Node
omega n k = Number (exp (0:+(2 * pi * fromIntegral(k))/fromIntegral(n)))
-- END GENERATION

-- SIMPLIFICATION
algSimp :: Node -> State Dag Node
algSimp (Number x) = snumM x
algSimp (Plus xs) = mapM algSimp xs >>= splusM
algSimp (Times (x, y)) = algSimp x >>= (\x' ->
                          algSimp y >>= (\y' ->
                            stimesM (x', y')))
algSimp (UMinus x) = algSimp x >>= suminusM
algSimp (Store (v, x)) = algSimp x >>= (\x' ->
                            memoize (Store (v, x')))
algSimp x = memoize x

snumM :: Complex Double -> State Dag Node
snumM x = let real = realPart x
              imag = imagPart x
              epsilon = 1e-8
              real' = if (abs real) < epsilon then 0.0 else real
              imag' = if (abs imag) < epsilon then 0.0 else imag
          in memoize (Number (real' :+ imag'))

stimesM :: (Node, Node) -> State Dag Node
stimesM (UMinus a, b) = stimesM (a, b) >>= suminusM
stimesM (a, UMinus b) = stimesM (a, b) >>= suminusM
stimesM (Number a, Number b) = snumM (a * b)
stimesM (Number a, Times (Number b, c)) = snumM (a * b) >>= (\x -> stimesM (x, c))
stimesM (Number a, b) = if (is_zero a) then snumM (0:+0)
                        else if (is_i a) then memoize (Times (Number a, b))
                        else if (is_one a) then memoize b
                        else if (is_mone a) then memoize b
                        else memoize (Times (Number a, b))
stimesM (a, Number b) = stimesM (Number b, a)
stimesM (a, b) = memoize (Times (a, b))

suminusM :: Node -> State Dag Node
suminusM (UMinus a) = memoize a
suminusM a = memoize (UMinus a)

splusM :: [Node] -> State Dag Node
splusM [] = snumM (0:+0)
splusM (x:[]) = algSimp x
splusM as = memoize (Plus (reduceSum as))

reduceSum :: [Node] -> [Node]
reduceSum [] = []
reduceSum [Number a] = if (is_zero a) then [] else [Number a]
reduceSum ((Number a):(Number b):rest) = reduceSum ((Number (a + b)):rest)
reduceSum ((Number a):b:rest) = b:(reduceSum ((Number a):rest))
reduceSum (a:rest) = a:(reduceSum rest)

is_zero :: Complex Double -> Bool
is_zero x = x == (0.0:+0.0)

is_one ::  Complex Double -> Bool
is_one x = x == (1.0:+0.0)

is_i ::  Complex Double -> Bool
is_i x = x == (0.0:+1.0)

is_mone ::  Complex Double -> Bool
is_mone x = x == ((-1.0):+0.0)

lookupM :: Node -> State Dag (Maybe Node)
lookupM n = do
  dag <- get
  let nodes = (nodeMap dag)
  return (M.lookup (hash_expr n) nodes)

insertM :: Node -> State Dag Node
insertM n = do
  dag <- get
  let nodes = (nodeMap dag)
  let newMap = M.insert (hash_expr n) n nodes
  put (dag {nodeMap = newMap})
  return n

memoize :: Node -> State Dag Node
memoize n = do
  look <- lookupM n
  case look of
    Nothing -> (insertM n >>= (\t -> return t))
    Just x -> return x

simplify :: [Node] -> State Dag Dag
simplify nodes = do

  eNodes <- mapM algSimp nodes
  dag <- get
  put (dag {nodes = eNodes})

  etNodes <- transpose eNodes

  ftNodes <- mapM algSimp etNodes
  dag <- get
  put (dag {nodes = ftNodes})

  fNodes <- transpose ftNodes

  gNodes <- mapM algSimp fNodes
  dag <- get
  put (dag {nodes = gNodes})

  dag <- get
  return dag
-- END SIMPLIFICATION

-- TRANSPOSE
transpose :: [Node] -> State Dag [Node]
transpose nodes = do
  dag <- get
  all_nodes <- return $ M.elems (nodeMap dag)
  cNodes <- mapM parents all_nodes
  load_nodes <- return $ L.filter is_load $ M.elems (nodeMap dag)
  trans_nodes <- mapM transposeM load_nodes
  dag <- get
  put (dag {nodes = trans_nodes, cMap = M.empty, nodeMap = M.empty})
  return trans_nodes

transposeM :: Node -> State Dag Node
transposeM node = do
  dag <- get
  maybe_children <- lookupC node
  myChildren <- get_my_children node maybe_children
  tChildren <- mapM transposeM myChildren
  tNode <- transform node (Plus tChildren)
  return tNode

get_my_children :: Node -> Maybe [Node] -> State Dag [Node]
get_my_children node maybe_nodelist = case maybe_nodelist of
  Just x -> return x
  Nothing -> return []

transform :: Node -> Node -> State Dag Node
transform node child = case node of
  Load x -> return (Store (x, child))
  Store (x, child) -> return (Load x)
  Times (Number a, x) -> return (Times (Number a, child))
  UMinus x -> return (UMinus child)
  Number x -> return node
  Plus nodes -> return child

lookupC :: Node -> State Dag (Maybe [Node])
lookupC child_node = do
  dag <- get
  let nodes = (cMap dag)
  return (M.lookup (hash_expr child_node) nodes)

insertC :: Node -> [Node] -> State Dag [Node]
insertC child_node node_list = do
  dag <- get
  let nodes = (cMap dag)
  let newMap = M.insert (hash_expr child_node) node_list nodes
  put (dag {cMap = newMap})
  return node_list

memoizeC :: Node -> Node -> State Dag [Node]
memoizeC node child = do
  look <- lookupC child
  case look of
    Nothing -> insertC child [node] >>= return
    Just xs -> insertC child (L.nub (node:xs)) >>= return

parents :: Node -> State Dag [Node]
parents node = case node of
  Store (v, n) -> insertC (Store (v, n)) [] >>= (\x' -> memoizeC node n)
  Plus l -> mapM (memoizeC node) l >>= (\x' -> return $ head x')
  Times (a, b) -> mapM (memoizeC node) [a,b]  >>= (\x' -> return $ head x')
  UMinus x -> memoizeC node x
  _ -> return [node]

is_load :: Node -> Bool
is_load = (\x -> case x of
  Load _ -> True
  _ -> False)

-- Testing
examp :: Int -> Dag
examp n = evalState (simplify $ outputs_n n) (init_dag $ outputs_n n)

complexity :: Dag -> ((String, Int), (String, Int), (String, Int), (String, Int), (String, Int), (String, Int))
complexity dag = foldl func (("Constants:", 0), ("Adds:", 0), ("Mults:", 0), ("Subs:", 0), ("Loads:", 0), ("Stores:", 0)) $ M.elems $ nodeMap dag
    where func (consts, adds, mults, subs, loads, stores) (Number xs) = (("Constants:", (snd consts) + 1), adds, mults, subs, loads, stores)
          func (consts, adds, mults, subs, loads, stores) (Plus xs) = (consts, ("Adds:", (snd adds) + (length xs) - 1), mults, subs, loads, stores)
          func (consts, adds, mults, subs, loads, stores) (Times xs) = (consts, adds, ("Mults:", (snd mults) + 1), subs, loads, stores)
          func (consts, adds, mults, subs, loads, stores) (UMinus xs) = (consts, adds, mults, ("Subs:", (snd subs) + 1), loads, stores)
          func (consts, adds, mults, subs, loads, stores) (Load xs) = (consts, adds, mults, subs, ("Loads:", (snd loads) + 1), stores)
          func (consts, adds, mults, subs, loads, stores) (Store xs) = (consts, adds, mults, subs, loads, ("Stores:", (snd stores) + 1))
-- End Testing
