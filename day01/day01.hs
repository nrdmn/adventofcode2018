import Data.Set
import System.Environment

readFreqs f = fmap parse . lines <$> readFile f
    where parse x
            | head x == '+' = read (tail x)
            | otherwise     = read x

firstDup l = firstDup' l empty
    where firstDup' l' known
            | (head l') `member` known = head l'
            | otherwise                = firstDup' (tail l') (insert (head l') known)

main = do
    args <- getArgs
    freqs <- readFreqs (head args)
    print $ sum freqs
    print $ firstDup (0:(scanl1 (+) (cycle freqs)))
