import System.Environment
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Map as Map

hasPlant rules prev pos = (a, b, c, d, e) `Set.member` rules
    where a = (pos-2) `IntSet.member` prev
          b = (pos-1) `IntSet.member` prev
          c = pos     `IntSet.member` prev
          d = (pos+1) `IntSet.member` prev
          e = (pos+2) `IntSet.member` prev

step rules gen = IntSet.fromList $ filter (hasPlant rules gen) [(IntSet.findMin gen)-2..(IntSet.findMax gen)+2]

normalize gen = (IntSet.findMin gen, IntSet.fromList $ (\x -> x - IntSet.findMin gen) <$> IntSet.toList gen)

findDup l = findDup' l Map.empty
    where findDup' ((gen, (offset, pattern)):xs) known
            | pattern `elem` (Map.keys known) = (gen, fst (known Map.! pattern), offset - snd (known Map.! pattern))
            | otherwise                       = findDup' xs $ Map.insert pattern (gen, offset) known

main = do
    args <- getArgs
    input <- lines <$> readFile (head args)
    let rules = Set.fromList $ (\x -> (x!!0, x!!1, x!!2, x!!3, x!!4)) <$> map (=='#') <$> (filter (\x -> x!!9 == '#') $ drop 2 input)
    let start = IntSet.fromList $ map fst $ filter snd $ zip [0..] $ (=='#') <$> (drop 15 $ head input)
    let generations = start : [step rules x | x <- generations]
    print $ sum $ IntSet.toList $ generations!!20
    let (second, first, offset) = findDup $ zip [0..] $ map normalize generations
    print $ (quot (50000000000-first) (second-first)) * (IntSet.size $ generations!!(first + ((50000000000-first) `mod` (second-first)))) + (sum $ IntSet.toList $ generations!!(first + ((50000000000-first) `mod` (second-first))))
