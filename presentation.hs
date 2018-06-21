import Data.Complex
import Data.Hashable
import System.IO
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
                  nodes = ns,
                  schedule = []
                }

type CExpr = (Node, Node)

data Dag = Dag {cMap :: DependencyMap, nodeMap :: ExprMap, nodes :: [Node],
                schedule :: [SNode]} deriving (Show)

data Node = Number Double | Load Var | Plus [Node] | Times (Node, Node) | UMinus (Node) | Store (Var, Node)
data Var = Temp Int | Re_Inp Int | Im_Inp Int | Re_Out Int | Im_Out Int

instance Show Node where
  show node = case node of
    Number x -> show x
    Load x -> show x
    Plus x -> "(+ " ++ unwords ([show n | n <- x]) ++ ")"
    Times (x, y) -> "(* " ++ show x ++ " " ++ show y ++ ")"
    UMinus x -> "-" ++ show x
    Store (x, y) -> "(Store " ++ show x ++ "=[ " ++ show y ++ " ])"

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
  (Re_Inp x) == (Re_Inp y) = x == y
  (Im_Inp x) == (Im_Inp y) = x == y
  (Re_Out x) == (Re_Out y) = x == y
  (Im_Out x) == (Im_Out y) = x == y
  _ == _ = False

instance Show Var where
  show var = case var of
    Temp x -> "temp" ++ show x
    Re_Inp x -> "input[" ++ show x ++ "].real"
    Im_Inp x -> "input[" ++ show x ++ "].imag"
    Re_Out x -> "output[" ++ show x ++ "].real"
    Im_Out x -> "output[" ++ show x ++ "].imag"
-- END DATA DEFS

-- HASHING FUNCTIONS
hash_expr :: Node -> Int
hash_expr (Number x) = 16719 * (hash x) + 7
hash_expr (Plus xs) = 5 + 991 * sum (map hash_expr xs)
hash_expr (Times (x, y)) = 41 + 31415 * (hash_expr x +  hash_expr y)
hash_expr (UMinus x) = 42 + 12345 * (hash_expr x)
hash_expr (Load v) = 1 + 1237 * hash_var v
hash_expr (Store (v, e)) = 2 * hash_var v + 2345 * hash_expr e

hash_var :: Var -> Int
hash_var (Temp x) = hash x
hash_var (Re_Inp x) = hash x * 47
hash_var (Im_Inp x) = hash x * 241 + 13
hash_var (Re_Out x) = hash x * 173
hash_var (Im_Out x) = hash x * 131 + 41
-- END HASHING FUNCTIONS

-- GENERATION
fftgen :: Int -> (Int -> CExpr) -> Int -> (Int -> CExpr)
fftgen 2 input sign = (\i -> if (even i)
  then (Plus [fst $ input i, fst $ input (i + 1)],
        Plus [snd $ input i, snd $ input (i + 1)])
  else (Plus [fst $ input (i - 1), UMinus (fst $ input i)],
        Plus [snd $ input (i - 1), UMinus (snd $ input i)]))
fftgen n input sign = cooley_tukey 2 (div n 2) input sign

load_input :: Int -> CExpr
load_input i = (Load (Re_Inp i), Load (Im_Inp i))

output_n_re :: Int -> Int -> Node
output_n_re n i = Store (Re_Out i, fst $ (fftgen n (load_input) (1)) i)

output_n_im :: Int -> Int -> Node
output_n_im n i = Store (Im_Out i, snd $ (fftgen n (load_input) (1)) i)


outputs_n :: Int -> [Node]
outputs_n n = L.foldl (\acc x -> acc ++ [(output_n_re n x),(output_n_im n x)]) [] [0..n-1]
  --[output_n_re n i| i <- [0..n-1]]

cooley_tukey :: Int -> Int -> (Int -> CExpr) -> Int -> (Int -> CExpr)
cooley_tukey n1 n2 input sign =
  let inner j2 = fftgen n1 (\j1 -> input (j1 * n2 + j2)) sign in
    let twiddle i1 j2 = comp_mult (omega (n1 * n2) (sign * i1 * j2)) (inner j2 i1) in
      let outer i1 = fftgen n2 (twiddle i1) sign in
        (\i -> outer (mod i n1) (div i n1))

comp_mult :: CExpr -> CExpr -> CExpr
comp_mult (re, im) (re', im') = (Plus [Times (re, re'), UMinus (Times (im, im'))],
                                 Plus [Times (re, im'), Times (im, re')])

omega :: Int -> Int -> CExpr
omega n k = let val = ((2 * pi * fromIntegral(k)) / fromIntegral(n))
                real = cos val
                imag = sin val
            in (Number real, Number imag)
-- END GENERATION

-- SIMPLIFICATION
algSimp :: Node -> State Dag Node
algSimp (Number x) = if x < 0 then snumM (abs x) >>= suminusM else snumM x
algSimp (Plus xs) = mapM algSimp xs >>= splusM
algSimp (Times (x, y)) = algSimp x >>= (\x' ->
                          algSimp y >>= (\y' ->
                            stimesM (x', y')))
algSimp (UMinus x) = algSimp x >>= suminusM
algSimp (Store (v, x)) = algSimp x >>= (\x' ->
                            memoize (Store (v, x')))
algSimp x = memoize x

snumM :: Double -> State Dag Node
snumM x = let epsilon = 1e-8
              num = if (abs x) < epsilon then 0.0 else x
          in if (is_zero num)
            then return $ Number 0.0
            else if (num < 0) then return (Number (abs num)) >>= suminusM
            else memoize $ Number num

stimesM :: (Node, Node) -> State Dag Node
stimesM (UMinus a, b) = stimesM (a, b) >>= suminusM
stimesM (a, UMinus b) = stimesM (a, b) >>= suminusM
stimesM (Number a, Number b) = snumM (a * b)
stimesM (Number a, Times (Number b, c)) = snumM (a * b) >>= (\x -> stimesM (x, c))
stimesM (Number a, b) = if (is_zero a) then return $ Number 0.0
                        else if (is_one a) then memoize b
                        else if (is_mone a) then suminusM b
                        else memoize (Times (Number a, b))
stimesM (a, Number b) = stimesM (Number b, a)
stimesM (a, b) = memoize (Times (a, b))

suminusM :: Node -> State Dag Node
suminusM (Number x) = if (is_zero x) then return $ Number 0.0 else memoize $ UMinus (Number x)
suminusM (UMinus a) = memoize a
suminusM a = memoize (UMinus a)

splusM :: [Node] -> State Dag Node
splusM [] = return $ Number 0.0
splusM (x:[]) = algSimp x
splusM as = memoize (Plus (reduceSum as))

reduceSum :: [Node] -> [Node]
reduceSum [] = []
reduceSum [Number a] = if (is_zero a) then [] else [Number a]
reduceSum [UMinus (Number a)] = if (is_zero a) then [] else [UMinus (Number a)]
reduceSum ((Number a):(Number b):rest) = reduceSum ((Number (a + b)):rest)
reduceSum ((Number a):(UMinus (Number b)):rest) = reduceSum ((Number (a - b)):rest)
reduceSum ((UMinus (Number a)):(Number b):rest) = reduceSum ((Number (b - a)):rest)
reduceSum ((UMinus (Number a)):(UMinus (Number b)):rest) = reduceSum ((UMinus (Number (a + b))):rest)
reduceSum ((Number a):b:rest) = if (is_zero a) then b:(reduceSum rest) else b:(reduceSum ((Number a):rest))
reduceSum ((UMinus (Number a)):b:rest) = if (is_zero a) then b:(reduceSum rest) else b:(reduceSum ((UMinus (Number a)):rest))
reduceSum (a:rest) = a:(reduceSum rest)

is_zero :: Double -> Bool
is_zero x = x == 0.0

is_one :: Double -> Bool
is_one x = x == 1.0

is_mone :: Double -> Bool
is_mone x = x == -1.0

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

is_store :: Node -> Bool
is_store = (\x -> case x of
  Store _ -> True
  _ -> False)

is_num :: ExprMap -> Int -> Bool
is_num nodemap = (\x -> case (M.lookup x nodemap) of
  Nothing -> False
  Just node -> case node of
    Number _ -> True
    _ -> False)
-- End Transpose

-- Scheduling
data Color = Black | Red | Blue deriving (Show, Enum, Eq)
data SNode = SNode {s_node :: Int, color :: Color, var :: String,
                    priority :: Int} deriving (Show, Eq)

initSchedule :: State Dag Dag
initSchedule = do
  dag <- get
  let nodes = M.elems (nodeMap dag)
  let stores = L.filter is_store nodes
  let loads = L.filter is_load nodes
  let others = L.filter (\x -> (not $ is_load x) && (not $ is_store x)) nodes
  let red = [SNode { s_node = hash_expr n, color = Red, var = show n,
                     priority = 1} | n <- loads]
  let blue = [SNode { s_node = hash_expr n, color = Blue, var = show n,
                      priority = 1} | n <- stores]
  let black = [SNode { s_node = hash_expr n, color = Black,
                       var = "temp" ++ (replace_minus $ show (hash_expr n)),
                       priority = -1} | n <- others]
  put (dag {schedule = (red ++ blue ++ black)})
  cNodes <- mapM parents nodes
  loop_red (red ++ blue ++ black)
  dag <- get
  return dag

loop_red :: [SNode] -> State Dag ()
loop_red before_blue = do
  dag <- get
  let sched_before = schedule dag
  red_cycle
  dag <- get
  let sched_after = schedule dag
  if (length $ L.filter (\x -> (color x) == Black) $ schedule dag) == 0
    then return ()
    else if ((subsetOf before_blue sched_after) && (subsetOf sched_after before_blue))
      then return ()
      else loop_blue sched_before

loop_blue :: [SNode] -> State Dag ()
loop_blue before_red = do
  dag <- get
  let sched_before = schedule dag
  blue_cycle
  dag <- get
  let sched_after = schedule dag
  if (length $ L.filter (\x -> (color x) == Black) $ schedule dag) == 0
    then return ()
    else if ((subsetOf before_red sched_after) && (subsetOf sched_after before_red))
      then return ()
      else loop_red sched_before

v_node :: (Int -> Bool) -> ExprMap -> Int -> [Int]
v_node v_func nmap n_hash = if (v_func n_hash)
  then [n_hash]
  else foldl (\acc x -> (v_node v_func nmap $ hash_expr x) ++ acc) [] $ predec n_hash nmap

get_node :: Int -> ExprMap -> Node
get_node h_val n_map = case M.lookup h_val n_map of
  Nothing -> error "no node"
  Just x -> x

red_cycle :: State Dag ()
red_cycle = do
  dag <- get
  let cur = (schedule dag)
  let red_sched = L.filter (\x -> (color x) == Red) cur
  let red_nodes = map s_node red_sched
  let new_priority = 1 + (L.maximum $ map priority red_sched)
  let black_nodes = L.filter (\x -> (color x) == Black) cur
  let c_map = (cMap dag)
  let n_map = (nodeMap dag)
  let valid_func = valid_dependencies n_map red_nodes
  let successors = foldl (\acc x -> (succec x c_map) ++ acc) [] red_nodes
  let valid_successors = L.filter (valid_func) successors
  let invalid_successors = L.filter (not . (valid_func)) successors
  let preds = foldl (\acc x -> v_node valid_func n_map x) [] invalid_successors
  let n_nodes_succ = L.filter (\x -> elem x valid_successors) $ (map s_node black_nodes)
  let n_nodes_pred = L.filter (\x -> elem x preds) $ (map s_node black_nodes)
  let mapped_succ = map (\x -> if ((elem (s_node x) n_nodes_succ) || (elem (s_node x) n_nodes_pred)) then (set_node x Red new_priority) else x) cur
  updated <- return $ mapped_succ
  dag <- get
  put (dag {schedule = updated})

succec :: Int -> DependencyMap -> [Int]
succec n_hash cmap = case (M.lookup n_hash cmap) of Just x -> map hash_expr x
                                                    Nothing -> []

blue_cycle :: State Dag ()
blue_cycle = do
  dag <- get
  let cur = (schedule dag)
  let blue_sched = L.filter (\x -> (color x) == Blue) cur
  let blue_nodes = map s_node blue_sched
  let new_priority = 1 + (L.maximum $ map priority blue_sched)
  let black_nodes = L.filter (\x -> (color x) == Black) cur
  let n_map = (nodeMap dag)
  let preds = foldl (\acc x -> (map hash_expr $ predec x n_map) ++ acc) [] blue_nodes
  let n_nodes = L.filter (\x -> elem x preds) $ (map s_node black_nodes)
  updated <- return $ map (\x -> if elem (s_node x) n_nodes then (set_node x Blue new_priority) else x) cur
  dag <- get
  put (dag {schedule = updated})

predec :: Int -> ExprMap -> [Node]
predec n_hash emap = case (M.lookup n_hash emap) of
  Just x -> case x of
    Number x -> []
    Plus xs -> xs
    Times (a, b) -> [a,b]
    UMinus a -> [a]
    Store (_, n) -> [n]
    Load _ -> []
  Nothing -> []

set_node :: SNode -> Color -> Int -> SNode
set_node snode s_color prio = SNode {s_node = (s_node snode), color = s_color, var = (var snode), priority = prio}

set_var :: SNode -> Int -> SNode
set_var snode int = SNode {s_node = (s_node snode), color = (color snode), var = "tmp" ++ show int, priority = (priority snode)}

valid_dependencies :: ExprMap -> [Int] -> Int -> Bool
valid_dependencies emap valid_hashes potential = subsetOf (map hash_expr $ predec potential emap) valid_hashes
-- End Scheduling

-- Generate C Code
type Temp = Int

generate_code :: Int -> Dag -> IO ()
generate_code n dag = do
  contents <- readFile "core.c"
  let filename = "fftw" ++ show n ++ ".c"
  let write = writeFile filename
  let append = appendFile filename
  write contents
  append $ "\tCOMP input[" ++ show n ++ "];\n"
  append $ "\tCOMP output[" ++ show n ++ "];\n"
  append $ "\tfor(int i = 0; i < " ++ show n ++ "; i++) {\n"
  append $ "\t\tinput[i].real = atof(argv[(2*i + 1)]);\n"
  append $ "\t\tinput[i].imag = atof(argv[(2*i + 2)]);\n"
  append $ "\t}\n"
  append $ "\tfor(int i = 0; i < " ++ show n ++ "; i++) {\n"
  append $ "\t\toutput[i].real = 0.0;\n"
  append $ "\t\toutput[i].imag = 0.0;\n"
  append $ "\t}\n"
  let sched = schedule dag
  let nodemap = nodeMap dag
  let red_sched = L.filter (\n -> (color n) == Red) sched
  let blue_sched = L.filter (\n -> (color n) == Blue) sched
  let update_blues = loop_update nodemap blue_sched
  mapM append $ map (var_load nodemap) $ L.sortBy (\x y -> (compare (priority x) (priority y))) red_sched
  mapM append $ map (var_load nodemap) $ L.reverse $ L.sortBy (\x y -> (compare (priority x) (priority y))) update_blues
  append $ "\tfor(int i = 0; i < " ++ show n ++ "; i++) {\n"
  append $ "\t\tprintf(\"%lf +i %lf\\n\", output[i].real, output[i].imag);\n"
  append $ "\t}\n"
  append "}"

get_min_priority :: SNode -> ExprMap -> [SNode] -> Int
get_min_priority the_snode n_map sched =
  let pred_ints = map hash_expr $ predec (s_node the_snode) n_map
      sched_preds = L.sortBy (\x y -> (compare (priority x) (priority y))) $ L.filter (\x -> elem (s_node x) pred_ints) sched
  in if (length sched_preds) > 0 then (priority $ head sched_preds) - 1 else priority the_snode

loop_update :: ExprMap -> [SNode] -> [SNode]
loop_update nodemap blue_sched =
  let new_sched = map (\x -> SNode {s_node = (s_node x), color = (color x), var = (var x), priority = (get_min_priority x nodemap blue_sched)}) blue_sched
  in if ((subsetOf new_sched blue_sched) && (subsetOf blue_sched new_sched)) then new_sched else loop_update nodemap new_sched

var_load :: ExprMap -> SNode -> [Char]
var_load nodemap snode = case (M.lookup (s_node snode) nodemap) of
  Just node -> case node of
    Number x -> "\tdouble " ++ (node_or_load $ Number x) ++ " = " ++ show x ++ ";\n"
    UMinus x -> "\tdouble " ++ (node_or_load $ UMinus x) ++ " = -" ++ (node_or_load x) ++ ";\n"
    Times (x, y) -> "\tdouble " ++ (node_or_load $ Times (x, y)) ++ " = " ++ (node_or_load x) ++ " * " ++ (node_or_load y) ++ ";\n"
    Plus xs -> "\tdouble " ++ (node_or_load $ Plus xs) ++ " = " ++ (node_sum xs) ++ ";\n"
    Load x -> ""
    Store (x, n) -> "\t" ++ (show x)  ++" = " ++ (node_or_load n) ++ ";\n"
  Nothing -> "ERROR\n"

node_sum :: [Node] -> String
node_sum xs = unwords $ L.intersperse "+" $ map node_or_load xs

replace_minus :: [Char] -> [Char]
replace_minus = map (\x -> if (x == '-') then '_' else x)

node_or_load :: Node -> String
node_or_load node = replace_minus $ case node of
  Load x -> show (Load x)
  _ -> "tmp" ++ show (hash_expr node)
-- End Generation

-- Testing
plan :: Int -> State Dag Dag
plan n = do
  simplify $ outputs_n n
  initSchedule

examp :: Int -> Dag
examp n = evalState (plan n) (init_dag $ outputs_n n)

gen :: Int -> IO ()
gen n = generate_code n $ examp n

complexity :: Dag -> ((String, Int), (String, Int), (String, Int), (String, Int), (String, Int), (String, Int))
complexity dag = foldl func (("Constants:", 0), ("Adds:", 0), ("Mults:", 0), ("Subs:", 0), ("Loads:", 0), ("Stores:", 0)) $ M.elems $ nodeMap dag
    where func (consts, adds, mults, subs, loads, stores) (Number xs) = (("Constants:", (snd consts) + 1), adds, mults, subs, loads, stores)
          func (consts, adds, mults, subs, loads, stores) (Plus xs) = (consts, ("Adds:", (snd adds) + (length xs) - 1), mults, subs, loads, stores)
          func (consts, adds, mults, subs, loads, stores) (Times xs) = (consts, adds, ("Mults:", (snd mults) + 1), subs, loads, stores)
          func (consts, adds, mults, subs, loads, stores) (UMinus xs) = (consts, adds, mults, ("Subs:", (snd subs) + 1), loads, stores)
          func (consts, adds, mults, subs, loads, stores) (Load xs) = (consts, adds, mults, subs, ("Loads:", (snd loads) + 1), stores)
          func (consts, adds, mults, subs, loads, stores) (Store xs) = (consts, adds, mults, subs, loads, ("Stores:", (snd stores) + 1))

full_complexity :: [Node] -> ((String, Int), (String, Int), (String, Int), (String, Int), (String, Int), (String, Int))
full_complexity nodes = L.foldl sum_complexity (("Constants:", 0), ("Adds:", 0), ("Mults:", 0), ("Subs:", 0), ("Loads:", 0), ("Stores:", 0)) $ L.map node_complexity nodes

node_complexity :: Node -> ((String, Int), (String, Int), (String, Int), (String, Int), (String, Int), (String, Int))
node_complexity node = case node of
  Number x -> (("Constants:", 1), ("Adds:", 0), ("Mults:", 0), ("Subs:", 0), ("Loads:", 0), ("Stores:", 0))
  Plus xs -> L.foldl sum_complexity (("Constants:", 0), ("Adds:", 1), ("Mults:", 0), ("Subs:", 0), ("Loads:", 0), ("Stores:", 0)) (L.map node_complexity xs)
  Times (a, b) -> L.foldl sum_complexity (("Constants:", 0), ("Adds:", 0), ("Mults:", 1), ("Subs:", 0), ("Loads:", 0), ("Stores:", 0)) ([node_complexity a, node_complexity b])
  UMinus x -> sum_complexity (("Constants:", 0), ("Adds:", 0), ("Mults:", 0), ("Subs:", 1), ("Loads:", 0), ("Stores:", 0)) $ node_complexity x
  Load x -> (("Constants:", 0), ("Adds:", 0), ("Mults:", 0), ("Subs:", 0), ("Loads:", 1), ("Stores:", 0))
  Store (a, b) -> sum_complexity (("Constants:", 0), ("Adds:", 0), ("Mults:", 0), ("Subs:", 0), ("Loads:", 0), ("Stores:", 1)) $ node_complexity b

sum_complexity :: ((String, Int), (String, Int), (String, Int), (String, Int), (String, Int), (String, Int)) ->
                  ((String, Int), (String, Int), (String, Int), (String, Int), (String, Int), (String, Int)) ->
                  ((String, Int), (String, Int), (String, Int), (String, Int), (String, Int), (String, Int))
sum_complexity (consts, adds, mults, subs, loads, stores) (consts', adds', mults', subs', loads', stores') =
  let c = (snd consts) + (snd consts')
      a = (snd adds) + (snd adds')
      m = (snd mults) + (snd mults')
      s = (snd subs) + (snd subs')
      l = (snd loads) + (snd loads')
      st =  (snd stores) + (snd stores')
  in ((fst consts, c), (fst adds, a), (fst mults, m), (fst subs, s), (fst loads, l), (fst stores, st))
-- End Testing
