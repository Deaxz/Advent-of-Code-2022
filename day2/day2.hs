import System.IO  
import Control.Monad
import Data.List 

-- 1. part
calculatescore = do  
                 handle <- openFile "input" ReadMode
                 contents <- hGetContents handle
                 let rounds = words contents
                 print (strategy rounds + bonusScore rounds)
                 print (strategy' . map tonum $ rounds)
                 print (highStrategy simplestrat . map tonum $ rounds)
                 print (highStrategy' simplestrat' rounds)
                 hClose handle  

-- 1. forsøg
strategy :: [String] -> Int
strategy [] = 0
strategy ("A":"X":xss) = 3 + strategy xss
strategy ("A":"Y":xss) = 6 + strategy xss
strategy ("A":_:xss) = 0 + strategy xss
strategy ("B":"X":xss) = 0 + strategy xss
strategy ("B":"Y":xss) = 3 + strategy xss
strategy ("B":_:xss) = 6 + strategy xss
strategy ("C":"X":xss) = 6 + strategy xss
strategy ("C":"Y":xss) = 0 + strategy xss
strategy ("C":_:xss) = 3 + strategy xss
strategy _ = 0 -- unnecessary case

bonusScore :: [String] -> Int
bonusScore [] = 0
bonusScore (opp:"X":xss) = 1 + bonusScore xss
bonusScore (opp:"Y":xss) = 2 + bonusScore xss
bonusScore (opp:"Z":xss) = 3 + bonusScore xss
bonusScore _ = 0 -- unnecessary case


-- 2. forsøg
strategy' :: [Int] -> Int
strategy' [] = 0
strategy' (i:j:xs) = decide i j + strategy' xs
         where decide x y | x == y = 3 + y
                          | max x y == y && y-x > 1 = 0 + y
                          | max x y == x && x-y > 1 = 6 + y
                          | x < y = 6 + y
                          | otherwise = 0 + y
strategy' _ = 0 -- unnecessary case

tonum :: String -> Int
tonum "A" = 1
tonum "B" = 2
tonum "C" = 3
tonum "X" = 1
tonum "Y" = 2
tonum "Z" = 3
tonum _ = 0 -- unnecessary case

-- 3. forsøg
highStrategy :: (Int -> Int -> Int) -> [Int] -> Int
highStrategy _ [] = 0
highStrategy f (x:y:xs) = f x y + highStrategy f xs
highStrategy _ _ = 0 -- unnecessary case

simplestrat :: Int -> Int -> Int 
simplestrat 1 3 = 0 + 3
simplestrat 3 1 = 6 + 1
simplestrat x y | x == y = 3 + y
                | x < y = 6 + y
                | otherwise = 0 + y

-- 4. forsøg
highStrategy' :: (String -> String -> Int) -> [String] -> Int
highStrategy' _ [] = 0
highStrategy' f (x:y:xs) = f x y + highStrategy' f xs
highStrategy' _ _ = 0 -- unnecessary case

simplestrat' :: String -> String -> Int
simplestrat' "A" "Z" = 0 + 3
simplestrat' "C" "X" = 6 + 1
simplestrat' x y | xnum == ynum = ynum + 3
                 | xnum < ynum = ynum + 6
                 | otherwise = ynum + 0
                 where xnum = tonum x
                       ynum = tonum y
    
-- 2. part
calculatescore' = do  
                  handle <- openFile "input" ReadMode
                  contents <- hGetContents handle
                  let rounds = words contents
                  print (highStrategy perfectstrat . map tonum $ rounds)
                  print (highStrategy' perfectstrat' rounds)
                  hClose handle  

-- 1. forsøg
perfectstrat :: Int -> Int -> Int
-- lose
perfectstrat 1 1 = 3 + 0
perfectstrat 2 1 = 1 + 0
perfectstrat 3 1 = 2 + 0
perfectstrat x 2 = x + 3
perfectstrat 1 3 = 2 + 6
perfectstrat 2 3 = 3 + 6
perfectstrat 3 3 = 1 + 6
perfectstrat _ _ = 0 -- unnecessary case

-- 2. forsøg
perfectstrat' :: String -> String -> Int
perfectstrat' x "X" | tonum x == 1 = 3 
                    | otherwise = tonum x - 1
perfectstrat' x "Y" = tonum x + 3 
perfectstrat' x "Z" | tonum x == 3 = 1 + 6 
                    | otherwise = tonum x + 1 + 6
perfectstrat' _ _ = 0 -- unnecessary case
