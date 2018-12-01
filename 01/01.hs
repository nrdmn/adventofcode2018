import Data.Set
import System.Environment

readFreqs f = do
    freqsStr <- fmap lines $ readFile f
    return (fmap parseInt freqsStr)
        where parseInt x = if (head x) == '+' then read (tail x) else read x

firstDup l = firstDup' l (fromList [])
    where firstDup' l' known
            | (head l') `member` known = head l'
            | otherwise                = firstDup' (tail l') (insert (head l') known)

main = do
    args <- getArgs
    freqs <- readFreqs (head args)
    print $ sum freqs
    print $ firstDup (0:(scanl1 (+) (cycle freqs)))
