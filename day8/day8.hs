import System.IO  
import Control.Monad
import Data.List 
import Data.String

main = do 
         handle <- openFile "input" ReadMode
         contents <- hGetContents handle
         --print (makematrix (lines contents))
         print (makematrix a)
         hClose handle  

-- Grid af træer
-- Lister af rows og strings af kolonner
--
-- For hvert tal i hver row skal jeg tjekke

--map xss, hvor xss [String] 

makematrix :: [String] -> [[Int]]
makematrix = map (\xs -> [(read [x] :: Int) | x <- xs])

visibletrees :: [[Int]] -> Int
visibletrees mat (i,j) = isvisible mat (i,j) 
                       where v = mat !! i !! j

isvisible :: [[Int]] -> Int -> Bool
isvisible mat (i,j) | length



                    where v = mat !! i !! j
                          visible = [elem | row <- mat !! i, elem <- row, elem <]

-- [[...], ...]
-- tag elem, kig i row og så i column


--visibletrees :: [[Int]] -> Int
--visibletrees mat (i,j,v) = map 
  --                       where hori = mat !! i 








--isvisible :: [Int] -> Bool
--isvisible ns = True






a = ["30373","25512","65332","33549","35390"]
