import System.Environment
import Data.List (maximumBy)
import Data.Function (on)

powerlevel sn (x,y) = quot ((rackID * y + sn) * rackID) 100 `mod` 10 - 5
    where rackID = x + 10

part1 sn = maximumBy (compare `on` power) [(x,y) | x <- [1..298], y <- [1..298]]
    where power (x,y) = sum [powerlevel sn (a,b) | a <- [x..x+2], b <- [y..y+2]]

part2 sn = maximumBy (compare `on` power) [(x,y,z) | x <- [1..300], y <- [1..300], z <- [1..300-(max x y)+1]]
    where power (x,y,z) = sum [powerlevel sn (a,b) | a <- [x..x+z-1], b <- [y..y+z-1]]

main = do
    args <- getArgs
    let input = read $ head args
    print $ part1 input
    print $ part2 input
