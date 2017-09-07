toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : (toDigitsRev (n `div` 10))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []     -- Do nothing to the empty list
doubleEveryOther (x:[])     = [x]    -- Do nothing to lists with a single element
doubleEveryOther (x:(y:zs)) = x : 2 * y : doubleEveryOther zs

sumDigits1 ::  [Integer] -> Integer
sumDigits1 x = foldl (+) 0  x

sumDigits :: [Integer] -> Integer
sumDigits x = foldl (+) 0 $ map sumDigits1 $ map toDigitsRev x

validate :: Integer -> Bool
validate x = if (sumDigits $ doubleEveryOther $ reverse $ toDigits x) `mod` 10 == 0 then True else False
