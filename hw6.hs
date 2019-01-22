fib :: Integer -> Integer
fib i = if i < 2
           then i
           else (fib (i - 1) + (fib (i - 2)))
    
fibs1 :: [Integer]
fibs1 = fmap fib [0..]


fibs2 :: [Integer]
      
fibs2 = let preds = (zip fibs2  (tail fibs2)) :: [(Integer, Integer)]
            rest = ( fmap (uncurry (+)) preds ) :: [Integer]
        in
                [0,1] ++ rest 
      
