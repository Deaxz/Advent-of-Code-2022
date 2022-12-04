import System.IO  
import Control.Monad
import Data.List 

-- 1. part
priority = do 
             handle <- openFile "input" ReadMode
             contents <- hGetContents handle
             let rucksacks = lines contents
             print (calcpriority rucksacks)
             hClose handle  

calcpriority :: [String] -> Int
calcpriority [] = 0
calcpriority (xs:xss) = commonelement zs ys + calcpriority xss
                 where (zs,ys) = splitAt (length xs `div` 2) xs

commonelement :: String -> String -> Int
commonelement (x:xs) ys = case find (==x) ys of
                           Just c -> topriority c
                           Nothing -> commonelement xs ys
commonelement _ _ = 0 -- unneccesary case

topriority :: Char -> Int
topriority x = case l of 
                Just i -> i + 1 
                Nothing -> case m of
                            Just j -> j + 1 + offset
                            Nothing -> -1
             where l = elemIndex x ['a'..'z']
                   m = elemIndex x ['A'..'Z']
                   offset = 26

-- 2. part
priority' = do 
             handle <- openFile "input" ReadMode
             contents <- hGetContents handle
             let rucksacks = lines contents
             print (calcpriority' rucksacks)
             hClose handle  

calcpriority' :: [String] -> Int
calcpriority' [] = 0
calcpriority' (xs:ys:zs:xss) = commonelement' xs ys zs + calcpriority' xss
calcpriority' _ = 0 -- unneccesary case

commonelement' :: String -> String -> String -> Int
commonelement' (x:xs) ys zs = case find (==x) ys of
                               Just i -> case find (==x) zs of 
                                          Just j -> topriority i
                                          Nothing -> commonelement' xs ys zs
                               Nothing -> commonelement' xs ys zs
commonelement' _ _ _ = 0 -- unneccesary case
