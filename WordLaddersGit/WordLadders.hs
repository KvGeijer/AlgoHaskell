import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

data Node = Node {word :: String, adjList :: [String]} deriving (Eq, Show) 

findAdjList :: String -> [String] -> [String]
findAdjList word wordList = foldl updateAdj [] wordList
        where updateAdj acc x = if  word /= x && recNeigh (tail word) x then x : acc else acc

recNeigh :: String -> String -> Bool
recNeigh [] (y:[]) = True
recNeigh [] _ = False
recNeigh (x:xs) ys = recNeigh xs $ delete x ys

doQueries :: Map.Map String Node -> [String] -> IO ()
doQueries _ [] = do return ()
doQueries nodeMap (from:to:rest) = do 
  let shortest = bfs (Map.delete from nodeMap) (Map.singleton from $ fromJust $ Map.lookup from nodeMap) to 0
  putStrLn $ if shortest > -1 then show shortest else "Impossible"
  doQueries nodeMap rest

bfs :: Map.Map String Node -> Map.Map String Node -> String -> Int -> Int
bfs nodeMap mapSet to len
    | Map.member to mapSet     = len
    | Map.null mapSet          = -1
    | otherwise                = bfs smallerNodeMap newMapSet to $ len + 1
            where foldAdjFunc word (mapIt, mapSetIt) = if Map.member word mapIt 
                    then (Map.delete word mapIt, Map.insert word (fromJust $ Map.lookup word mapIt) mapSetIt) 
                    else (mapIt, mapSetIt)
                  foldFunc node (mapIt, mapSetIt) = foldr foldAdjFunc (mapIt, mapSetIt) $ adjList node
                  (smallerNodeMap, newMapSet) = Map.fold foldFunc (nodeMap, Map.empty) mapSet
            
main = do
  contents <- getContents
  let content = words $ contents
      n = read $ head content :: Int
  let (words, queries) = splitAt n $ drop 2 content
      nodeMap = Map.fromList [(word , Node word $ findAdjList word words)| word <- words]
  doQueries nodeMap queries

      
      

