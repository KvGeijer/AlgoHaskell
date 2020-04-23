import qualified Data.List as List
import qualified Control.Monad as Mon
import Text.Printf


type Coord = (Int,Int)

main = do
        --content <- readFile "./data/secret/4larger.in"
        content <- getContents
        let result = solve.toCoord.tail.words $ content :: Double
        printf "%.6f\n" result

--------- Utility functions

compareX, compareY :: Coord -> Coord -> Ordering
compareX (x1,_) (x2,_) = compare x1 x2
compareY (_,y1) (_,y2) = compare y1 y2

compareC :: (Coord, Coord) -> (Coord, Coord) -> Ordering
compareC p1 p2 = compare (uncurry cLength p1) $ uncurry cLength p2

toCoord :: [String] -> [Coord]
toCoord [] = []
toCoord (x:y:xs) = (read x :: Int, read y :: Int) : toCoord xs

cLength :: (Floating a, Ord a) => Coord -> Coord -> a
cLength (x1,y1) (x2,y2) = sqrt.fromIntegral $ (x1-x2)^2 + (y1-y2)^2

minMaybe :: Ord a => a -> Maybe a -> a
minMaybe x Nothing = x
minMaybe x (Just y) = min x y


-------- Solving

solve :: (Floating a, Ord a) => [Coord] -> a
solve list = let sortX = List.sortBy compareX list
                 sortY = List.sortBy compareY list
             in shortestPair (length list) sortX sortY


shortestPair :: (Floating a, Ord a) => Int -> [Coord] -> [Coord] -> a
shortestPair size
    | size >= 4         = shortestPairRec size
    | otherwise         = shortestPairDir

shortestPairRec :: (Floating a, Ord a) => Int -> [Coord] -> [Coord]  -> a
shortestPairRec size sortX sortY = let halfL = size `div` 2
                                       halfR = size - halfL
                                       (leftX, rightX) = splitAt halfL sortX
                                       splitX = fst.head $ rightX
                                       (leftY, rightY) = List.partition (\(x,_) -> x < splitX) sortY
                                       shortestLR = min (shortestPair halfL leftX leftY) $ shortestPair halfR rightX rightY    --Kan skriva min i mitten infix och skippa parenteser?
                                       mids = filter (\(x,_) -> (abs.fromIntegral) (x - splitX) < 2 * shortestLR ) sortY
                                       shortestMid = closestPairMid mids
                                   in minMaybe shortestLR shortestMid

shortestPairDir :: (Floating a, Ord a) => [Coord] -> [Coord] -> a
shortestPairDir list _
        | length list == 1 = 0
        | otherwise        = uncurry cLength $ head $ List.sortBy compareC $ [(x,y)| x <- list, y <- list, x /= y] --list >>= (\c1 -> list >>= \c2 -> Mon.guard (c1/=c2) >>  return (c1,c2))

closestPairMid :: (Floating a, Ord a) => [Coord] -> Maybe a
closestPairMid []  = Nothing
closestPairMid [x] = Nothing
closestPairMid [x,y] = Just $ cLength x y
closestPairMid (x:xs) = Just $ minMaybe (midRec x xs 11) (closestPairMid xs)


midRec :: (Floating a, Ord a) => Coord -> [Coord] -> Int -> a
midRec x [y] _          = cLength x y
midRec x (y:ys) count
                | count > 1         = min (cLength x y) (midRec x ys $ count - 1)
                | otherwise         =      cLength x y
