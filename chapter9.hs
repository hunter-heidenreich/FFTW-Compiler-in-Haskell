import System.Random
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
-- Functions of IO
-- Basic
  -- putStr - like putStrLn without the \n
  -- putChar - puts out a single char
  -- print - actually is printStrLn . show
  -- getChar - gets a single character (once the return key is pressed)
  -- when - Allows simplification of if blocks for IO
  -- sequence - given a list of IO actions, returns the result
  -- mapM - Maps a function and returns an IO list
  -- mapM_ - similar to above, but will toss the IO list
  -- forever - takes an IO action and loops it forver
  -- forM - reversed inputs for mapM functionality
  -- getContents - Reads all input from stdin until it hits an end file
  -- interact - takes a function String -> String and returns an IO action, runs the function, and prints the result
-- Files and streams
  -- openFile - takes a filepath and a IO mode, returns an IO handle
    -- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
  -- hGetContents - takes a handle and returns an IO String
  -- hClose - given a handle, closes a file that was opened
  -- withFile - takes a filepath, an IOMode, and a function that will return an IO action
  -- hGetLine, hPutStr, hPutStrLn, hGetChar - handles instead of stdin
  -- readFile - Reads an entire file into a IO string (lazily)
  -- writeFile - Takes a filepath and a string, and writes it to that file
  -- appendFile - like writeFile but appends instead of writes
  -- hSetBuffering - changes the size of buffer read from a handle
  -- hFlush - Given a handle, will flush the associated buffer
  -- openTempFile - opens a temporary file
  -- removeFile - deletes a file
  -- renameFile - renames a file
-- CL
  -- getArgs - gets the command line arguments
  -- getProgName - gets the name of the program
-- Random
  -- random - given a generator, will return a random value and a new generator
  -- mkStdGen - creates a random generator
    -- random (mkStdGen 100) :: (Int, StdGen)
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)
  -- randoms - gives an infinite sequence based on a generator
    -- take 5 $ randoms (mkStdGen 11) :: [Int]
  -- randomR - generates a random value in a range
  -- randomRs - generates an infinite stream in a range
  -- getStdGen - gets a StdGen from the system
  -- newStdGen - gets a new StdGen
-- Bytestring
  -- pack - takes a list of bytes and creates a ByteString
  -- unpack - inverse of pack
  -- fromChunks - takes a list of bytestrings and converts them to a lazy bytestring
  -- toChunks - takes a lazy bytestring and converts to a list of strings
  -- cons - takes a byte and puts it at the begining of a bytestring
  -- cons' - for taking a lot of bytestrings and adding them to the front
  -- empty - creates an empty bytestring
  -- all the main list functions have parallels here
  -- readFile - reads file in as bytestrings
-- Exceptions
  -- doesFileExist - returns if a file exists
  -- catch - a function that will take a try situation and function to handle if things go wrong
  -- isDoesNotExistError - One of many IOErrors
  -- ioError - generic IOError
  -- ioeGetFileName - allows program to Maybe see the FilePath that created an error
