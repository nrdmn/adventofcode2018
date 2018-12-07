import System.Environment
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
import Data.Char
import Data.Ord

rmdups = Set.toList . Set.fromList

part1 g
    | null g    = []
    | otherwise = next : (part1 $ filter (/=next) <$> Map.delete next g)
    where next = head $ Map.keys $ Map.filter (=="") g

part2 n graph = maximum $ snd $ part2' (fmap (\x -> (x, Nothing)) graph, (replicate n 0))
    where part2' (graph, workers)
            | Map.null $ pending graph = (graph, workers)
            | otherwise                = part2' (Map.insert next (fst $ graph Map.! next, Just nextEnds) graph, List.sort $ nextEnds : (tail workers))
            where readyAt task
                    | (fst $ task) == [] = Just 0
                    | otherwise          = maximum <$> sequence (map (\x -> snd (graph Map.! x)) $ fst $ task)
                  duration name = ord name - 4
                  pending = Map.filter (isNothing . snd)
                  next = fst $ List.minimumBy (comparing $ snd) $ map (\(x, y) -> (x, fromJust y)) $ filter (isJust . snd) $ Map.assocs $ readyAt <$> pending graph
                  nextEnds = duration next + max (head workers) (fromJust $ readyAt (graph Map.! next))

main = do
    args <- getArgs
    input <- map (\x -> (x!!5, x!!36)) <$> lines <$> readFile (head args)
    let graph = let all     = rmdups $ (map fst input) ++ (map snd input)
                    pending = foldl (\acc x -> Map.insertWith (++) (snd x) [(fst x)] acc) Map.empty input
                    done    = filter (\x -> Map.notMember x pending) all
                    in foldl (\acc x -> Map.insert x "" acc) pending done
    print $ part1 graph
    print $ part2 5 graph
