import System.Environment

first (x,_,_,_) = x
second (_,x,_,_) = x
third (_,_,x,_) = x
fourth (_,_,_,x) = x

bounds l = foldl (\acc x -> (min (fst x) (first acc), min (snd x) (second acc), max (fst x) (third acc), max (snd x) (fourth acc))) (fst $ head m, snd $ head m, fst $ head m, snd $ head m) m
    where m = map fst l

dim l = let b = bounds l in (abs (third b - first b), abs (fourth b - second b))

area l = uncurry (*) $ dim l

move = map (\(pos, vel) -> ((fst pos + fst vel, snd pos + snd vel), vel))

parse = map (\x -> ((x!!1, x!!2), (x!!4, x!!5))) . (map (map read)) . map words . lines . map (\x -> if x `elem` ['<', '>', ','] then ' ' else x)

minmsg l = minmsg' 0 l
    where minmsg' t l
            | (area $ move l) < (area l) = minmsg' (t+1) (move l)
            | otherwise                  = (t, l)

draw l = unlines $ fmap (\pos -> if pos `elem` (map fst l) then '#' else '.') <$> [[(x, y) | x <- [first $ bounds l .. third $ bounds l]] | y <- [second $ bounds l .. fourth $ bounds l]]

main = do
    args <- getArgs
    input <- parse <$> readFile (head args)
    let result = minmsg input
    putStr $ draw $ snd result
    print $ fst result
