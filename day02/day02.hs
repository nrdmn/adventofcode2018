import System.Environment
import qualified Data.Map.Strict as Map

countOccurences l = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty l
containsTwice l = Map.filter (\x -> x == 2) (countOccurences l) /= Map.empty
containsThrice l = Map.filter (\x -> x == 3) (countOccurences l) /= Map.empty
diff a b = foldl (\acc x -> acc + (fromEnum $ fst x /= snd x)) 0 (zip a b)
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

main = do
    args <- getArgs
    ids <- lines <$> readFile (head args)
    print $ uncurry (*) $ foldl (\acc x -> (fst acc + fromEnum (containsTwice x), snd acc + fromEnum (containsThrice x))) (0, 0) ids
    print $ map fst $ filter (\x -> fst x == snd x) $ uncurry zip $ head $ filter (\x -> diff (fst x) (snd x) == 1) (pairs ids)
