import System.IO  
import Control.Monad
import Data.List 

-- 1. part
mostcalories = do  
                 handle <- openFile "input.txt" ReadMode
                 contents <- hGetContents handle
                 print (findmaxcal contents)
                 hClose handle  
             where findmaxcal = maximum . buildElfList . lines

buildElfList :: [String] -> [Int] 
buildElfList [] = [] 
buildElfList xss = elf : buildElfList (nextelf xss)
                 where elf = sum . map read . takeWhile (/="") $ xss
                       nextelf = drop 1 . dropWhile (/="") 

-- 2. part
top3mostcalories = do  
                     handle <- openFile "input.txt" ReadMode
                     contents <- hGetContents handle
                     print (findtop3 contents)
                     hClose handle 
                 where findtop3 = sum . take 3 . reverse . sort . buildElfList . lines
