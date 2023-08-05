import System.IO  
import Control.Monad
import Data.List 
import Data.String
import Parsing

-- 1. part
main = do 
         handle <- openFile "input" ReadMode
         contents <- hGetContents handle
         let pairs = lines contents
         let pairs' = words contents
         print (drop 10 pairs)
         --print (take 100 paIirs')
         hClose handle  

{-
parsecon :: [String] -> [String]
parsecon (xs:xss) = 
                  where column = (length . takeWhile (==' ') xs) `div` 4

conlines :: [String] -> [Int]
conlines (" ":xs:xss) = parseints
conlines (_:xs:xss) = []

parseints :: Parser [Int]
parseints = do integer
-}

a = ["    [D]    ","[N] [C]    ","[Z] [M] [P]"," 1   2   3 ","move 1 from 2 to 1","move 3 from 1 to 3","move 2 from 2 to 1","move 1 from 1 to 2"]
b = drop 4 a




parseinstruct :: [String] -> (Int,Int,Int)
parseinstruct (xs:xss) = (num,from,to)
                      where (_,num,from,to) = splitOn xs




