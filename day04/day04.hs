import System.Environment
import qualified Data.IntMap.Strict as Map
import Data.List
import Data.Char (isDigit)

chunk [] = []
chunk l = (take chunklen l) : (chunk $ drop chunklen l)
    where chunklen = (length $ takeWhile (\x -> x!!19 /= 'G') $ drop 1 l) + 1

parse l = parse2 <$> parse1 <$> sort <$> foldl (\acc x -> Map.insertWith (++) (read $ drop 1 $ (words $ head x)!!3 :: Int) (tail x) acc) Map.empty l
    where parse1 []      = []
          parse1 (a:b:c) = (readMin a, (readMin b) - (readMin a)) : (parse1 c)
          replace x
              | isDigit x = x
              | otherwise = ' '
          readMin x = read ((words $ map replace x)!!4) :: Int
          parse2 = foldl (\acc x -> foldl (\acc' y -> Map.insertWith (+) y 1 acc') acc [fst x .. (fst x) + (snd x) - 1]) Map.empty

sleepyMinute x = foldl (\acc x -> if (snd x) > (snd acc) then x else acc) (0, 0) <$> Map.assocs <$> Map.filter (not . null) x
sleepiest x = fst $ foldl (\acc x -> if (snd x) > (snd acc) then x else acc) (0, 0) $ Map.assocs $ fmap (Map.foldl (\acc x -> acc + x) 0) x
mostSleepInAMinute x = (fst f, fst $ snd f)
    where f = foldl (\acc x -> if (snd $ snd x) > (snd $ snd acc) then x else acc) (0,(0,0)) $ Map.assocs $ sleepyMinute x


main = do
    args <- getArgs
    schedules <- parse <$> chunk <$> sort <$> lines <$> readFile (head args)
    print $ (sleepiest schedules) * (fst $ (sleepyMinute schedules) Map.! (sleepiest schedules))
    print $ uncurry (*) $ mostSleepInAMinute schedules
