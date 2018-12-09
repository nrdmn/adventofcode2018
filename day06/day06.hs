import System.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List

first (x,_,_,_) = x
second (_,x,_,_) = x
third (_,_,x,_) = x
fourth (_,_,_,x) = x

distance a b = abs (fst a - fst b) + abs (snd a - snd b)
bounds l = foldl (\acc x -> (min (fst x) (first acc), min (snd x) (second acc), max (fst x) (third acc), max (snd x) (fourth acc))) (fst $ head l, snd $ head l, fst $ head l, snd $ head l) $ tail l
closest p l = if (length $ filter (\x -> snd x == snd smallest) distances) == 1 then Just (fst smallest) else Nothing
    where distances = map (\x -> (x, distance p x)) l
          smallest  = foldl1 (\acc x -> if (snd x) < (snd acc) then x else acc) $ distances
distanceMap a b = map (\x -> (fst x, fromJust $ snd x)) $ filter (isJust . snd) $ map (\x -> (x, closest x a)) b

main = do
    args <- getArgs
    input <- fmap (\x -> (x!!0, x!!1)) <$> fmap (\x -> fmap read $ words $ filter (/=',') x :: [Int]) <$> lines <$> readFile (head args)
    let inside = [(x,y) | x <- [first $ bounds input .. third $ bounds input], y <- [second $ bounds input .. fourth $ bounds input]]
    let outside = let o1 = [(x,(second $ bounds input)-1) | x <- [(first $ bounds input)-1 .. (third $ bounds input)+1]]
                      o2 = [(x,(fourth $ bounds input)+1) | x <- [(first $ bounds input)-1 .. (third $ bounds input)+1]]
                      o3 = [((first $ bounds input)-1, y) | y <- [(second $ bounds input)-1 .. (fourth $ bounds input)+1]]
                      o4 = [((third $ bounds input)+1, y) | y <- [(second $ bounds input)-1 .. (fourth $ bounds input)+1]]
                      in o1 ++ o2 ++ o3 ++ o4
    let infinites = nub $ snd <$> distanceMap input outside
    let insideDistanceMap = filter (\x -> (snd x) `notElem` infinites) $ distanceMap input inside
    print $ maximum <$> fmap snd <$> Map.toList $ foldl (\acc x -> Map.insertWith (+) (snd x) 1 acc) Map.empty insideDistanceMap
    print $ length $ filter (<10000) $ map (\x -> sum $ map (distance x) input) inside
