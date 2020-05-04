
import Data.Array
import Data.Char
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

---------- main

main = do
    --(charMap, costs, queries) <- fmap parseInput $ readFile "./data/sample/1.in"  -- Test a single file
    (charMap, costs, queries) <- fmap parseInput getContents
    mapM (printQuery.doQuery costs charMap) queries
    --let optArrays = map (uncurry (optArray costs charMap)) queries                -- These two lines are to demonstate how long time optArray takes
    --mapM (\arr -> (putStrLn . show . (arr !) . snd . bounds) arr ) optArrays


---------- Solving the queries

printQuery :: (String, String) -> IO ()
printQuery (x, y) = do
    putStr x
    putStrLn $ ' ':y

doQuery :: Array Int (Array Int Int) -> Map.Map Char Int -> (String, String) -> (String,String)
doQuery costs charMap (string1, string2) = result where
    optArr = optArray costs charMap string1 string2
    (n,m)  = (length string1, length string2)
    difres = complStrings costs charMap optArr (listArray (1,n) string1) (listArray (1,m) string2) n m
    result = let (x,y) = difres in (reverse x, reverse y)

-- I have created a monster.... I could have encoded this in optArr as Writer values, but would it be worth it?
-- This is very ugly. But it also takes a trivial amount of time so I won't change it.
complStrings :: Array Int (Array Int Int) -> Map.Map Char Int -> Array (Int,Int) Int -> Array Int Char -> Array Int Char -> Int -> Int ->  (String, String)
complStrings costs charMap optArr string1 string2 i j
    | i == 0 && j == 0                                    = ("", "")
    | j == 0                                              = let (s1, s2) = curried (i-1) j in (char1:s1, '*':s2)
    | i == 0                                              = let (s1, s2) = curried i (j-1) in ('*':s1, char2:s2)
    | getCost costs charMap (string1 ! i) (string2 ! j) + optArr ! (i-1, j-1) == optArr ! (i,j)     = let (s1, s2) = curried (i-1) (j-1) in (char1:s1, char2:s2)          -- This line is not nice. But not necceceary either. Just to get the tests right, is the same as "otherwise" but the tests wanted it this way.
    | optArr ! (i,j) == (skipCost + optArr ! (i, j-1))    = let (s1, s2) = curried i (j-1) in ('*':s1, char2:s2)
    | optArr ! (i,j) == (skipCost + optArr ! (i-1, j))    = let (s1, s2) = curried (i-1) j in (char1:s1, '*':s2)
    | otherwise                                           = let (s1, s2) = curried (i-1) (j-1) in (char1:s1, char2:s2)
        where curried = complStrings costs charMap optArr string1 string2
              char1 = string1 ! i
              char2 = string2 ! j



---------- Dynamic algorithm
-- This very clearly seems to be what takes such a long time. BUT IT IS SO NICELY WRITTEN :((
-- I don't see how to do this better.

skipCost = -4

optArray :: Array Int (Array Int Int) -> Map.Map Char Int -> String -> String -> Array (Int,Int) Int
optArray costs charMap string1 string2 = optArr where
    (n, m) = (length string1, length string2)
    opt 0 0 = 0                 -- Not neccesary
    opt i 0 = i*skipCost
    opt 0 j = j*skipCost
    opt i j = maximum   [skipCost + optArr ! (i, j-1),
                         skipCost + optArr ! (i-1, j),
                         getCost costs charMap (string1A ! i) (string2A ! j) + optArr ! (i-1, j-1)]

    string1A = listArray (1, n) string1
    string2A = listArray (1, m) string2
    bounds = ((0,0), (n,m))
    optArr = ($!) listArray bounds [opt i j | (i,j) <- range bounds]



---------- Parsing the input

parseInput :: String -> (Map.Map Char Int, Array Int (Array Int Int), [(String,String)])
parseInput string = (charMap, costArray, rest) where
        strings             = lines string
        size                = length.words.head $ strings
        charMap             = lineToCharMap.head $ strings
        costArray           = linesToArray size . take size. tail $ strings
        rest                = linesToQueries.drop (2+size) $ strings

lineToCharMap :: String -> Map.Map Char Int
lineToCharMap = Map.fromList . flip zip [1..] . map head . words

lineToArray :: Int -> String -> Array Int Int
lineToArray size = listArray (1,size) . (map read.words)

-- This is not how 2-d arrays should look like in Haskell. Should do over and better by indexing (Int, Int). Like in optArray
linesToArray :: Int -> [String] -> Array Int (Array Int Int)
linesToArray size = listArray (1,size) . map (lineToArray size)

linesToQueries :: [String] -> [(String, String)]
linesToQueries = map lineToQuery

lineToQuery :: String -> (String,String)
lineToQuery string = let [x,y] = words string in (x,y)

----------- Acceccing from arrays

-- This is just because my bad way of doing the 2d array...
get :: (Int,Int) -> Array Int (Array Int a) -> a
get (x,y) array = array ! x ! y

-- Is this maybe a bottleneck?
getCost ::  Array Int (Array Int Int) -> Map.Map Char Int -> Char -> Char -> Int
getCost array mapp char1 char2 = get (ind1,ind2) array where
        ind1 = Maybe.fromJust $ Map.lookup char1 mapp
        ind2 = Maybe.fromJust $ Map.lookup char2 mapp



---------------- Naive test
test1 = do
    (charMap, costs, queries) <- fmap parseInput $ readFile "./data/sample/1.in"
    let optArr = uncurry (optArray costs charMap) (head queries)
    putStrLn.show $ optArr ! (1,1)
    putStrLn.show $ optArr ! (1,2)
    putStrLn.show $ optArr ! (2,1)
    putStrLn.show $ getCost costs charMap 'C' 'C'
    mapM (printQuery.doQuery costs charMap) [("AABC","ABC")]
