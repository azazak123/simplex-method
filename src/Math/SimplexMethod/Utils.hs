module Math.SimplexMethod.Utils where

import Data.List (transpose)

replace :: Int -> a -> [a] -> [a]
replace n a arr = take n arr ++ [a] ++ drop (n + 1) arr

determinant :: Num a => [[a]] -> a
determinant [[x]] = x
determinant [[a11, a12], [a21, a22]] = a11 * a22 - a12 * a21
determinant (x : xs) = sum . zipWith f [0 ..] $ x
  where
    f index element = (-1) ^ (index + 1) * element * determinant (transpose (take index (transpose xs) ++ drop (index + 1) (transpose xs)))
determinant _ = 0

roundToDigit :: (Integral b, RealFrac a) => b -> a -> a
roundToDigit n number = (/ 10 ^ n) . fromInteger . round $ 10 ^ n * number
