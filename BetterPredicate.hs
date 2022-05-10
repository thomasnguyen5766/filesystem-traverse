import System.IO
import Control.Exception (handle, bracket)
import GHC.IO.Exception
import System.Directory (Permissions, getDirectoryContents, doesDirectoryExist, getPermissions, getModificationTime)
import Data.Time
import Control.Monad
import System.FilePath



getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)


simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

{-
  2 problems: 
   - IF the input of `hFileSize` is not a file --> Throw exception that crashes the app
   - "File is not closing properly" hClose never be called - The file never closes util garbage collectors run -> consume resources - Linux : 1024 files open simultaneously
    --  the files opened cannot be reached by other programs
-}


{-
  hFilesize :: Handle -> IO Integer -- this function will throw exeception if the input is not a plain file

  -0- we want a function will return Nothing if the input is not a file otherwise return the size of file

    saferFileSize :: FilePath -> IO (Maybe Integer)

    with the help of `handle` clause --- `catch` clause - to catch any excetion will be thrown

    1 Problem remain - the second one  
-}

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle ((\_  -> return Nothing) :: IOError -> IO (Maybe Integer)) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)


-- --- acquire - use - Release Pattern ---
bracket' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket' = undefined
{-
  3 arguments: 
  - 1st -- action acquires a resource
  - 2nd -- action to release resource
  - 3rd -- action run in between

  if the "1st" action succeeds - the "release - 2nd" always called
-}

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $ do
  bracket (openFile path ReadMode) hClose  $ \h -> do
    size <- hFileSize h
    return (Just size)



{- 'Need Analyse -- later'
  The order in which we call bracket and handle is important -- catching exception must always on the outermost in the order of calling --
  -- handle always make sure to return  `Nothing` if there is wrong with IO actions
  -- if we swich bracket <--> handle - then 
-}
getFileSize' :: FilePath -> IO (Maybe Integer)
getFileSize' path = bracket (openFile path ReadMode) hClose  $ \h ->
  handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $ do
    size <- hFileSize h
    return (Just size)


betterFind :: InfoP Bool -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

-- -- *Predicates* ---

-- embeded Domain Specific for Predicate

-- Domain specific -- use native facilities to write code that solve some narrow problems

type InfoP a = FilePath
                -> Permissions
                -> Maybe Integer
                -> UTCTime
                -> a

-- InfoP a :: Is a function that receive 4 arguments and return a value of type `a` -- any type

-- extracts the path from the arguments passed to a Predicate
pathP :: InfoP FilePath
-- pathP = \path -> \_ -> \_ -> \_ -> path 
pathP path _ _ _ = path


sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k
{- equalP analysing:

  --  equalP works by returning anonymous function which takes the arguments `Accepted by a Predicate - 4 args` - passes
    those arguments to the Predicate - f - compares them with k - values

  -- equalP construct a Predicates - returns a Function

  -- equalP emphasis the fact that We think of it as taking two arguments - but actually 6 args
    ---> equalP'
-}
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

greaterP' :: (Eq a, Ord a) => InfoP a -> a -> InfoP Bool
greaterP' f k w x y z = f w x y z > k

-- betterFind (sizeP `equalP` 1024) :: FilePath -> IO [FilePath]

-- -- General with lifting --
-- 1. -- 
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP f g k = \w x y z -> g w x y z `f` k

equalP'' :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP'' f k = liftP (==) f k
equalP'' = liftP (==)

greaterP'' :: (Eq a, Ord a) => InfoP a -> a -> InfoP Bool
greaterP'' = liftP (>)

-- 2. --
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
-- liftP2 f g k = \w x y z -> g w x y z `f` k w x y z
liftP2 f g k w x y z = g w x y z `f` k w x y z


equalP2 :: (Eq a) => InfoP a -> InfoP a -> InfoP Bool
-- equalP2 g k = liftP2 (==) g k
equalP2 = liftP2 (==)

-- liftP2 is more general than liftP
-- We can implement liftP from liftP2
constP :: a -> InfoP a
-- constP  = \x -> \_ -> \_ -> \_ -> \_ -> x 
constP x _ _ _ _ = x

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' f g k = liftP2 f g (constP k)

-- -----
{- Combinators :
  Function that take other functions as arguments and returning nre functions 
-}

-- -- Begining Helper functions --

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)

orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

-- --- End of Helper functions

-- ------- *** ---------
test :: (Ord a, Num a) => FilePath -> p1 -> Maybe a -> p2 -> Bool
test path _ (Just size) _ = takeExtension path == ".cpp" && size > 1000 
test _ _ _ _ = False

test2 :: InfoP Bool
test2 = (liftPath takeExtension `equalP''` ".hs") `andP` (sizeP `greaterP''` 1000)
