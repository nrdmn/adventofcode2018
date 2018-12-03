import System.Environment
import qualified Data.IntMap.Strict as Map
import Data.Char (isDigit)

hash (a,b) = 1000*a + b

data Square = Square {sqid :: Int, left :: Int, top :: Int, width :: Int, height :: Int } deriving (Show)

parseSquare x = Square (sq!!0) (sq!!1) (sq!!2) (sq!!3) (sq!!4)
    where sq = read <$> (words $ map replace x)
          replace x
            | not (isDigit x) = ' '
            | otherwise       = x

coverage s = foldl (\acc x -> cover x acc) Map.empty s
    where area x = [(a,b)|a<- [left x .. left x + width x - 1], b <- [top x .. top x + height x - 1]] 
          cover sq a = foldl (\acc x -> Map.insertWith (+) (hash x) 1 acc) a (area sq)

main = do
    args <- getArgs
    squares <- (fmap parseSquare) <$> lines <$> readFile (head args)
    print $ length $ Map.filter (> 1) (coverage squares)
    print $ sqid . fst <$> filter (\x -> Map.isSubmapOf (snd x) (coverage squares)) (map (\x -> (id x, coverage [x])) squares)
