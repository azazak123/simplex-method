module Main (main) where

import Math.SimplexMethod
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main =
  if and . zipWith (==) answers . fmap getSolution $ tests then exitSuccess else exitFailure

tests =
  [ simplexMethod
      [[5 :: Double, -2], [-1, 2], [1, 1]]
      ["<=", "<=", ">="]
      [7, 5, 6]
      [1, 2],
    simplexMethod
      [[2, -5], [4, 3], [10, -2]]
      ["<=", ">=", ">="]
      [3, 12, 10]
      [3, 5],
    simplexMethod [[3, 1, 1, 1, 1], [2, -1, 3, 0, 0], [0, 5, 6, 1, 0]] ["=", "=", "="] [5, 4, 11] [5, -1, 1, 0, 0]
  ]

answers = [[3 :: Double, 4], [], [1, 1, 1]]
