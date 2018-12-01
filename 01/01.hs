readFreqs f = do
    freqsStr <- fmap lines $ readFile f
    return (fmap parseInt freqsStr)
        where parseInt x = if (head x) == '+' then read (tail x) else read x

findDupFreq cur freqs known =
    if next `elem` known then
        next
    else
        findDupFreq next (tail freqs) (next:known)
    where
        next = cur + (head freqs)

main = do
    freqs <- readFreqs "input"
    print $ sum freqs
    print $ findDupFreq 0 (cycle freqs) [0]
