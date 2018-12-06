import System.Environment
import Data.Char

reduce x
    | (length x) /= (length $ r x) = reduce $ r x
    | otherwise                    = x
    where r (x:y:xs)
              | (isLower x) && (isUpper y) && (x == (toLower y)) = r xs
              | (isUpper x) && (isLower y) && ((toLower x) == y) = r xs
              | otherwise                                        = x : (r (y:xs))
          r (a) = a

main = do
    args <- getArgs
    input <- filter isLetter <$> readFile (head args)
    print $ length $ reduce input
    print $ foldl1 min $ (\c -> length $ reduce $ filter (\d -> (toLower d) /= c) input) <$> ['a'..'z']
