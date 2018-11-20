import Data.List
    
fun1piecemeal :: [Integer] -> Integer
fun1piecemeal [] = 1
fun1piecemeal (x:xs)
    | even x = (x - 2) * fun1piecemeal xs
    | otherwise = fun1piecemeal xs




fun1 :: [Integer] -> Integer
fun1 l = foldl (*) 1 $ fmap (subtract 2) $ filter even l


fun2piecemeal :: Integer -> Integer
fun2piecemeal 1 = 0
fun2piecemeal n | even n = n + fun2piecemeal (n `div` 2)
                | otherwise = fun2piecemeal (3 * n + 1)

fun2iter :: Integer -> Integer              
fun2iter n | even n = n `div` 2
          | otherwise = 3 * n + 1


                        
fun2 :: Integer -> Integer
fun2 n = sum $ filter even $ takeWhile (\i -> i > 1) $ iterate fun2iter n

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree a = foldr addNode Leaf a
            
addNode :: a -> Tree a -> Tree a
addNode a Leaf = Node 0 Leaf a Leaf
addNode a (Node h left v right) = if (height left) < (height right)
                                  then let nn = (addNode a left) in
                                       Node (max (height nn) (height right) + 1) nn v right
                                  else let nn = (addNode a right) in
                                       Node (max (height left) (height nn) + 1) left v nn

height :: Tree a -> Integer
height Leaf = -1
height (Node h left v right) = h

-- foldTree "ABCDEFGHIJ"            


 -- exercise 3: odd number of true values

xor :: [Bool] -> Bool
xor = foldl (/=) False 


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> (f a) : b) []

         


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> f y x) base (reverse xs)
                    

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let crossoutify = filter (\i -> i <= n) . fmap crossout
                      pairs = cartProd [1..n] [1..n]

                      crossouts = crossoutify pairs
                      combined = [1..n] ++ crossouts
                      justfirst = fmap head
                      dubinc n = n*2 + 1
                      filterer = filter (\x -> (length x == 1)) in

                   ((fmap dubinc) . justfirst . filterer . group . sort) combined

                                  


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]                    

incpairs :: [(Integer, Integer)]
incpairs = [(i,j) | j <- [1..], i <- [1..j]]            

crossout :: (Integer , Integer) -> Integer
crossout (i, j) = i + j + 2*i*j
               
