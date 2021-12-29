toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ zipWith (\x y -> if odd x then 2 * y else y) [0 ..] (reverse x)

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ concatMap toDigits x

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther $ toDigits x) `mod` 10 == 0

main :: IO ()
main = do
  print (validate 4012888888881882)
  