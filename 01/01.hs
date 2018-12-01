import Data.Set
import System.Environment

readFreqs f = do
    freqsStr <- fmap lines $ readFile f
    return (fmap parseInt freqsStr)
        where parseInt x = if (head x) == '+' then read (tail x) else read x

findDupFreq cur freqs known =
    if next `member` known then
        next
    else
        findDupFreq next (tail freqs) (insert next known)
    where
        next = cur + (head freqs)

main = do
    args <- getArgs
    freqs <- readFreqs (head args)
    print $ sum freqs
    print $ findDupFreq 0 (cycle freqs) (fromList [0])
