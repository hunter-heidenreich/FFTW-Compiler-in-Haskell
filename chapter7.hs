-- Imports --
import qualified Data.List as List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube  
--import Data.List (nub, sort)                    Only these two functinos
--import Data.List hiding (nub)                   Not this function
--import qualified Data.Map                       Avoid namespace clashes Data.Map.<name>
--import qualified Data.Map as M                  Functions are now M.<name>
numUniques :: (Eq a) => [a] -> Int
numUniques = length . List.nub

-- Data.List Functions:
  -- intersperse - Puts an element inbetween elements of a list
  -- intercalate - Inserts a list into elements of a list (flattens)
  -- transpose - Performs a transpose operation on a list of lists
addCubicLists :: (Num a) => [[a]] -> [a]
addCubicLists cubics = List.map sum $ List.transpose cubics -- [[0,3,5,9],[10,0,0,9],[8,5,1,-1]] = 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1
  -- foldl', foldl1' - Strict folds (no lazy evaluation to create trunks)
  -- concat - Flattens a list of lists
  -- concatMap - Maps a function to a list and then flattens the lists generated
  -- and - Returns whether only true values in a list of bools
  -- or - Returns whether any true values in a list of bools
  -- any, all - Take a predicate and return whether any (or all) elements satisfy it
  -- iterate - Creates an infinite list by take a function and a
  -- splitAt - Splits a list at a given index into two parts
  -- takeWhile - Takes elements from a list until they fail a predicate
  -- dropWhile - Same as takeWhile but it drops and returns the rest
  -- span - Like takeWhile but returns two lists, the takeWhile component and the rest
  -- break - Like span, but it breaks the list in half where it first is true
  -- sort - Sorts a list of the Ord typeclass
  -- group - Given a list, groups equal elements into sublists
  -- inits and tails - Creates a list of lists by decreasing elements in a list till they are empty
  -- isInfixOf, isSuffixOf, isPrefix of - searches for sublist inside list
  -- elem, notElem - Returns whether elem is in or not in
  -- partition - Given a list and a predicate, gives two lists based on whether they statisfy
  -- find - Given a list and a predicate, returns a Maybe value (the first one encountered)
  -- elemIndex - Searches for an element and returns a Maybe int
  -- elemIndices - Returns the indices where an element is present
  -- findIndex, findIndices - Similar to above
  -- zip3, zip4 - Like zip but with more lists (goes up to zip7)
  -- zipWith3, zipWith4 - Likewise (up to zipWith7)
  -- lines - splits lines of input based on \n
  -- unlines - reverse of lines process
  -- words, unwords - Splits words into lists, reverses that process
  -- nub - Keeps unique elements
  -- delete - Deletes first instance of an element
  -- \\ - Like the \ operator for sets
  -- union - Performs a union operation
  -- intersect - Performs an intersect operation
  -- insert - Inserts an element in the last spot where it is greater than or equal to elements to the left
  -- genericLength, genericTake, genericDrop, genericSplitAt, genericIndex, genericReplicate - Return Num typeclass instead of Int
  -- nubBy, deleteBy, unionBy, intersectBy, groupBy - Also take an equality function insead of just ==
  -- sortBy, insertBy, maximumBy, minimumBy - Take functions that determine order

-- Data.Char Functions:
  -- Lots of predicates
  -- toUpper, toLower, toTitle
  -- digitToInt, intToDigit - For hex
  -- ord - to view ordinal value
encode :: Int -> String -> String
encode shift msg =
    let ords = List.map ord msg
        shifted = List.map (+ shift) ords
    in  List.map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- Data.Map Functions:
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = List.foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
  -- fromList - Takes a list and returns an association map (removing duplicates)
  -- empty - Creates an empty Map
  -- insert - Inserts a key value pair
  -- null - checks if null
  -- size - returns size
  -- singleton - takes a key and a value and returns a map of just one pair
  -- lookup - looks for key value
  -- member - checks if member is in map
  -- map, filter - work like list functions
  -- toList - converts to list
  -- keys, elems - returns lists of keys or elems
  -- fromListWith - allows duplicate keys, so long as its told how to handle them
  -- insertWith - supplied a function for dealing with duplicates

-- Data.Set Functions:
  -- fromList - Creates a set from a list
  -- intersection, difference, union
  -- null, size, member, empty, singleton, insert, delete
  -- map, filter
  -- toList
  -- setNub
