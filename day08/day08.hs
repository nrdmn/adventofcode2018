import System.Environment
import Data.Maybe

data Node = Node [Node] [Int] deriving (Show)

class Size a where
    size :: a -> Int

instance Size Node where
    size (Node children metadata) = 2 + size children + length metadata

instance Size [Node] where
    size = sum . map size

parse (x:y:xs) = let
    children = children' x xs
        where children' 0 x = []
              children' n x = parse x : (children' (n-1) (drop (size $ parse x) x))
    metadata = take y $ drop (size children) xs
    in Node children metadata

part1 (Node c m) = sum m + (sum $ map part1 c)

(!?) l n
    | n < length l = Just (l !! n)
    | otherwise    = Nothing

part2 (Node [] m) = sum m
part2 (Node c m) = sum $ part2 <$> (catMaybes $ map (\x -> c !? (x-1)) m)

main = do
    args <- getArgs
    input <- parse <$> fmap read <$> words <$> readFile (head args)
    print $ part1 input
    print $ part2 input
