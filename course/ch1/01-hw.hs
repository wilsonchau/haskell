-- 1
toDigits :: Integer -> [Integer]
toDigits n
    | n < 10 = [n]
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 10 = [n]
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs)
    | length zs `mod` 2 == 0 = x*2 : y   : doubleEveryOther zs
    | otherwise              = x   : y*2 : doubleEveryOther zs

-- make it point free???
-- 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum (toDigits x)
sumDigits (x:ys) = sumDigits [x] + sumDigits ys

-- how to point free???
-- come back after we learn
-- 4
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0
{-validate x = sumDigits . doubleEveryOther . toDigits x `mod` 10 == 0-}

-- 5
-- Towers of Hanoi
-- Skip for now
