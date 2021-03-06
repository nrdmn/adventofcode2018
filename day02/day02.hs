import System.Environment
import qualified Data.Map.Strict as Map

countOccurences l = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty l
containsN n l = Map.filter (== n) (countOccurences l) /= Map.empty
diff a b = sum $ map (\x -> fromEnum $ uncurry (/=) x) (zip a b)
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

main = do
    args <- getArgs
    ids <- lines <$> readFile (head args)
    print $ uncurry (*) $ foldl (\(acc1, acc2) x -> (acc1 + fromEnum (containsN 2 x), acc2 + fromEnum (containsN 3 x))) (0, 0) ids
    print $ fst $ unzip $ filter (uncurry (==)) $ uncurry zip $ head $ filter (\x -> 1 == uncurry diff x) (pairs ids)
