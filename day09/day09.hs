import System.Environment
import Data.List
import qualified Data.Sequence as Seq

data Game = Game { scores :: Seq.Seq Int, circle :: Seq.Seq Int, current :: Int } deriving (Show)

play game marble
    | marble `mod` 23 == 0 =
        game {
            scores  = Seq.adjust (+ (circle game `Seq.index` nextpos23 + marble)) (marble `mod` (length $ scores game)) (scores game),
            circle  = Seq.deleteAt nextpos23 (circle game),
            current = nextpos23
        }
    | otherwise =
        game {
            circle  = Seq.insertAt nextpos marble (circle game),
            current = nextpos
        }
        where nextpos = (current game + 1) `mod` (Seq.length (circle game)) + 1
              nextpos23 = (current game - 7) `mod` (Seq.length (circle game))

part1 (players, marbles) = maximum $ scores $ foldl' play (Game (Seq.replicate players 0) (Seq.singleton 0) 0) [1..marbles]

part2 (players, marbles) = part1 (players, marbles*100)

main = do
    args <- getArgs
    input <- (\x -> (x!!0, x!!6)) <$> fmap read <$> words <$> readFile (head args)
    print $ part1 input
    print $ part2 input
