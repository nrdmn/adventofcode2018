import System.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List

distance (a1, a2) (b1, b2) = abs (a1 - b1) + abs (a2 - b2)

bounds l = foldl (\(acc1, acc2, acc3, acc4) (x1, x2) -> (min x1 acc1, min x2 acc2, max x1 acc3, max x2 acc4)) (h1, h2, h1, h2) $ tail l
    where (h1, h2) = head l

closest p l = if (length $ filter (\x -> snd x == snd smallest) distances) == 1 then Just (fst smallest) else Nothing
    where distances = map (\x -> (x, distance p x)) l
          smallest  = foldl1 (\acc x -> if (snd x) < (snd acc) then x else acc) $ distances

distanceMap a b = map (\x -> (fst x, fromJust $ snd x)) $ filter (isJust . snd) $ map (\x -> (x, closest x a)) b

main = do
    args <- getArgs
    input <- fmap (\x -> (x!!0, x!!1)) <$> fmap (\x -> fmap read $ words $ filter (/=',') x :: [Int]) <$> lines <$> readFile (head args)
    let inside = let (b1, b2, b3, b4) = bounds input in [(x,y) | x <- [b1..b3], y <- [b2..b4]]
    let outside = let o1 = [(x, b2-1) | x <- [b1-1 .. b3+1]]
                      o2 = [(x, b4+1) | x <- [b1-1 .. b3+1]]
                      o3 = [(b1-1, y) | y <- [b2-1 .. b4+1]]
                      o4 = [(b3+1, y) | y <- [b2-1 .. b4+1]]
                      in o1 ++ o2 ++ o3 ++ o4
                      where (b1, b2, b3, b4) = bounds input
    let infinites = nub $ snd <$> distanceMap input outside
    let insideDistanceMap = filter (\x -> (snd x) `notElem` infinites) $ distanceMap input inside
    print $ maximum <$> fmap snd <$> Map.toList $ foldl (\acc x -> Map.insertWith (+) (snd x) 1 acc) Map.empty insideDistanceMap
    print $ length $ filter (<10000) $ map (\x -> sum $ map (distance x) input) inside
