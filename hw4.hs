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

fun2rec :: Integer -> Integer              
fun2rec n | even n = n `div` 2
          | otherwise = 3 * n + 1


                        
fun2 :: Integer -> Integer
fun2 n = sum $ filter even $ takeWhile (\i -> i > 1) $ iterate fun2rec n
