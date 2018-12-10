import System.Environment
import Data.Char

reduce l = foldl reduce' [] l
    where reduce' [] x = [x]
          reduce' acc@(y:_) x
            | x /= y && (toLower x) == (toLower y) = tail acc
            | otherwise                            = x:acc

main = do
    args <- getArgs
    input <- filter isLetter <$> readFile (head args)
    print $ length $ reduce input
    print $ foldl1 min $ (\c -> length $ reduce $ filter (\d -> (toLower d) /= c) input) <$> ['a'..'z']
