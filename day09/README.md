# Day 9
My first attempt with lists used about 5 GB of RAM for the first part. For part 2 I changed it to use sequences instead, which still requires about 3 GB.

Because containers >= 0.5.8 wasn't installed on my system, I used cabal.
```
cabal update
cabal install --only-dependencies
cabal run input
```
```
398048
3180373421
```
