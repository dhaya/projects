module Main where

import System.IO


wordCount :: Eq a => [a] -> [(a, Int)]
wordCount [] = []
wordCount (x:xs) = update x wc
  where wc = wordCount xs
        update x wc = let present = elem x $ map fst wc
                      in if present then
                           map (\(e, c) -> if (e == x)
                                           then (e, c + 1)
                                           else (e, c)) wc
                         else (x, 1) : wc


main :: IO ()
main = readFile "hask.cabal" >>= print . wordCount . words
