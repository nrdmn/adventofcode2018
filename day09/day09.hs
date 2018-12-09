import System.Environment
import Data.List

insertAt pos i l = let (xs,ys) = splitAt pos l in xs ++ [i] ++ ys
updateAt pos i l = let (xs,ys) = splitAt pos l in xs ++ [i] ++ (tail ys)
removeAt pos l = let (xs,ys) = splitAt pos l in xs ++ (tail ys)
addAt pos i l = updateAt pos (l!!pos + i) l

data Game = Game { scores :: [Int], circle :: [Int], current :: Int } deriving (Show)

play game marble
    | marble `mod` 23 == 0 =
        game {
            scores  = addAt (marble `mod` (length $ scores game)) (circle game !! nextpos23 + marble) (scores game),
            circle  = removeAt nextpos23 (circle game),
            current = nextpos23
        }
    | otherwise =
        game {
            circle  = insertAt nextpos marble (circle game),
            current = nextpos
        }
        where nextpos = (current game + 1) `mod` (length (circle game)) + 1
              nextpos23 = (current game - 7) `mod` (length (circle game))

part1 (players, marbles) = maximum $ scores $ foldl' play (Game (replicate players 0) [0] 0) [1..marbles]

main = do
    args <- getArgs
    input <- (\x -> (x!!0, x!!6)) <$> fmap read <$> words <$> readFile (head args)
    print $ part1 input
