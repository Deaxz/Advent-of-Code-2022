import System.IO  
import Control.Monad
import Data.List 
import Data.String

main = do 
         handle <- openFile "input" ReadMode
         contents <- hGetContents handle
         print (match 4 contents) -- 1. part
         print (match 14 contents) -- 2. part
         hClose handle  

match :: Int -> String -> Int
match n xs | all (<2) (occur curlist curlist) = n 
           | otherwise = 1 + match n (tail xs)
                     where curlist = take n xs

occur :: String -> String -> [Int]
occur [] xs = []
occur (c:xs) ys = (length . filter (==c)) ys : occur xs ys
