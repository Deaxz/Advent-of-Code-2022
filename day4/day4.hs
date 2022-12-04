import System.IO  
import Control.Monad
import Data.List 
import Data.String

-- 1. part
overlap = do 
            handle <- openFile "input" ReadMode
            contents <- hGetContents handle
            let pairs = lines contents
            print (sum . map (checkoverlap cmp) $ pairs)
            print (sum . map (checkoverlap cmp') $ pairs)
            hClose handle  

checkoverlap :: ((Int,Int) -> (Int,Int) -> Int) -> String -> Int
checkoverlap f xs = f (lower1,upper1) (lower2,upper2)
                  where
                    toint s = read s :: Int
                    (first, second) = case elemIndex ',' xs of
                                        Just i -> splitAt i xs
                                        Nothing -> undefined
                    (lower1,upper1) = case elemIndex '-' first of 
                                        Just i -> let (l1,u1) = splitAt i first
                                                  in (toint l1, toint $ drop 1 u1)
                                        Nothing -> undefined
                    (lower2,upper2) = case elemIndex '-' (drop 1 second) of
                                        Just i -> let (l2,u2) = splitAt i (drop 1 second)
                                                  in (toint l2, toint $ drop 1 u2)
                                        Nothing -> undefined

cmp :: (Int,Int) -> (Int,Int) -> Int
cmp (l1,u1) (l2,u2) | l1 <= l2 && u1 >= u2 = 1
                    | l1 >= l2 && u1 <= u2 = 1
                    | otherwise = 0

-- 2. part
cmp' :: (Int,Int) -> (Int,Int) -> Int
cmp' (l1,u1) (l2,u2) | u1 < l2 = 0
                     | l1 > u2 = 0
                     | otherwise = 1

