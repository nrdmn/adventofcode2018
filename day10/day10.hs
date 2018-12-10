import System.Environment

bounds l = foldl (\(acc1, acc2, acc3, acc4) (x1, x2) -> (min x1 acc1, min x2 acc2, max x1 acc3, max x2 acc4)) (h1, h2, h1, h2) m
    where m        = map fst l
          (h1, h2) = head m

dim l = (abs (b3 - b1), abs (b4 - b2))
    where (b1, b2, b3, b4) = bounds l

area l = uncurry (*) $ dim l

move = map (\((pos1, pos2), vel@(vel1, vel2)) -> ((pos1+vel1, pos2+vel2), vel))

parse = map (\x -> ((x!!1, x!!2), (x!!4, x!!5))) . (map (map read)) . map words . lines . map (\x -> if x `elem` ['<', '>', ','] then ' ' else x)

minmsg l = minmsg' 0 l
    where minmsg' t l
            | (area $ move l) < (area l) = minmsg' (t+1) (move l)
            | otherwise                  = (t, l)

draw l = unlines $ fmap (\pos -> if pos `elem` (map fst l) then '#' else '.') <$> [[(x, y) | x <- [b1..b3]] | y <- [b2..b4]]
    where (b1, b2, b3, b4) = bounds l

main = do
    args <- getArgs
    input <- parse <$> readFile (head args)
    let result = minmsg input
    putStr $ draw $ snd result
    print $ fst result
