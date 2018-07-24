import Data.List
    
nskips :: Int -> [a] -> [a]
--nskips _ [] = []          
nskips n l = if (length l)  <= n
             then []
             else head (drop n l) : nskips n (drop (n + 1) l)
                  
nskips2 :: Int -> [a] -> [a]
nskips2 n l = map fst $ filter (\(a,b) -> b `mod` (n + 1) == 0) $ zip l [1..]           
    -- number of items in return list is same as number of characters in the argument
skips :: [a] -> [[a]]
skips l = map (`nskips2` l) [0..((length l) - 1)]

f :: [Integer] -> Bool
f (a:b:c:_) = a < b && b > c


              
lm :: [[Integer]] -> [Integer]
lm ll = map (head . tail) $ filter f  ll
         
-- (\(a:b:c:_) -> a < b && b > c)
localMaxima :: [Integer] -> [Integer]
localMaxima l = let ll = take ((length l) - 2) (tails l) :: [[Integer]] in
                lm ll
-- histogram                
-- fill_missing :: [Integer] -> [[Integer]] -> [[Integer]]
-- fill_missing [] _ = []
-- fill_missing (x : xs) (y : ys) | x == y = y : fill_missing xs ys
--                                | otherwise = [] : fill_missing xs (y : ys)

-- get_row :: Integer -> [[Integer]] -> String
-- get_row i xs = fmap (\j -> if length j >= i then '*' else ' ') xs

-- get_rows ::
               
-- histogram :: [Integer] -> String
-- histogram l = let g = group $ sort l
--                   max_count = foldl max 0 $ map fst g -- histogram height
--                   patched = fill_missing [0..9] g
--                   mainhist = foldl (++) "" $ fmap get_rows 
                

string_of_count :: Int -> Int -> String
string_of_count j i = take (fromIntegral i)  (repeat '*') ++ take (fromIntegral (j - i)) (repeat ' ')

histogram :: [Integer] -> String
histogram l = let g = fmap (\c -> (length c) -1) $ group $ sort $ l ++ [0..9]
                  m = foldl max 0 g
                  mainhist = reverse $ transpose $ fmap (string_of_count m) g
                  barline = (take 10 (repeat '=')) :: String
                  labels = concat $ fmap show [0..9] :: String
              in
                unlines $ mainhist ++ [barline] ++  [labels]

main :: IO ()
main = print $ histogram [3,3,5]
