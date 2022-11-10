{-# LANGUAGE NamedFieldPuns #-}

module Math.SimplexMethod (step, simplexMethod, toTable, getSolution) where

import Data.Foldable (find)
import Data.List (transpose)
import Data.Maybe (fromJust)
import Math.SimplexMethod.Utils
import qualified Text.PrettyPrint.Boxes as BX

{- Start simplex method
Example:
simplexMethod [[5, -2],[-1, 2],[1,1]] ["<=","<=",">="] [7,5,6] [1,2] means:

  5x1 - 2x2 <= 7
  -x1 + 2x2 <= 5
  x1 + x2 >= 6

  f(x1,x2) = x1 + 2x2
-}
simplexMethod :: (RealFrac a, Enum a) => [[a]] -> [String] -> [a] -> [a] -> Simplex a
simplexMethod a symbols b c = loop initial
  where
    initial =
      Simplex
        { basis = [length c + greaterNumber + 1 .. length c + length a + greaterNumber],
          nonBasis = [1 .. length c + greaterNumber],
          table,
          initialFunc = c,
          limited = True
        }
    table = transpose (transpose a ++ greaterColumns ++ [b]) ++ [fmap negate newC]
    newC = foldl1 (zipWith (+)) a ++ replicate greaterNumber (-1) ++ [sum b]
    greaterNumber = length . filter (== ">=") $ symbols
    greaterColumns =
      fmap ((\i -> replace i (-1) (take (length a) [0, 0 ..])) . fst)
        . filter ((== ">=") . snd)
        . zip [0 ..]
        $ symbols

-- | Get solution
getSolution :: RealFrac a => Simplex a -> [a]
getSolution Simplex {basis, table, initialFunc, limited}
  | not limited = []
  | otherwise =
      roundToDigit (10 :: Int)
        . (\i -> solutions !! (subtract 1 . fromJust . find (== i + 1) $ basis))
        <$> [ 0
              .. subtract 1
                . length
                . filter (/= 0)
                $ initialFunc
            ]
  where
    solutions =
      init
        . last
        . transpose
        $ table

-- | Loops which makes steps while Simplex is changeable
loop :: RealFrac a => Simplex a -> Simplex a
loop initial
  | initial /= new = loop new
  | otherwise = initial
  where
    new = step initial

data Simplex a = Simplex
  { basis :: [Int],
    nonBasis :: [Int],
    table :: [[a]],
    initialFunc :: [a],
    limited :: Bool
  }
  deriving (Eq, Show)

-- | Simplex method main action
step :: RealFrac a => Simplex a -> Simplex a
step initial@Simplex {basis, nonBasis, table, initialFunc}
  | all (>= 0) free
      && all (>= 0) func
      && ( and
             . zipWith
               ( \f arr ->
                   (roundToDigit (10 :: Int) f == 0)
                     <= any
                       ( (> 0)
                           . roundToDigit (10 :: Int)
                       )
                       arr
               )
               func
             $ transpose table
         ) =
      initial
  | indexes == (-1, -1) = initial {limited = False}
  | otherwise = Simplex {basis = newBasis, nonBasis = newNonBasis, table = newTable, initialFunc, limited = True}
  where
    isMinusFree = or . fmap checkMinusFree $ init table
    isMinusFunc = or . fmap checkMinusFunc . init $ transpose table
    indexes
      | isMinusFree = (getY id x1, x1)
      | isMinusFunc = (getY (filter ((> 0) . snd)) x2, x2)
      | otherwise = (-1, -1)
      where
        x1 =
          fst
            . fromJust
            . find ((< 0) . snd)
            . zip [0 :: Int ..]
            . fromJust
            . find checkMinusFree
            . init
            $ table
        x2 =
          fst
            . fromJust
            . find (checkMinusFunc . snd)
            . zip [0 :: Int ..]
            . init
            . transpose
            $ table
        getY f xarg =
          snd
            . minimum
            . filter ((> 0) . fst)
            . zipWith (\a (i, b) -> (a / b, i)) free
            . f
            . zip [0 :: Int ..]
            . init
            $ transpose table !! xarg
    x = snd indexes
    y = fst indexes
    pivot = table !! y !! x

    newTable = transpose . replace x pivotColumn . transpose . replace y pivotRow $ others
    pivotRow = replace x (1 / pivot) . fmap (/ pivot) $ table !! y
    pivotColumn = replace y (1 / pivot) . fmap (negate . (/ pivot)) $ transpose table !! x
    others = fmap deterY . take (length table) $ [0 ..]

    deterY y1 = fmap deterX . take (length $ head table) $ [0 ..]
      where
        deterX x1 = determinant [[table !! y1 !! x1, table !! y1 !! x], [table !! y !! x1, pivot]] / pivot

    newBasis = replace y (nonBasis !! x) basis
    newNonBasis = replace x (basis !! y) nonBasis

    checkMinusFree arr = roundToDigit (10 :: Int) (last arr) < 0 && any ((< 0) . roundToDigit (10 :: Int)) (init arr)
    checkMinusFunc arr = roundToDigit (10 :: Int) (last arr) < 0 && any ((> 0) . roundToDigit (10 :: Int)) (init arr)
    free = last $ transpose table
    func = last table

-- | Render Simplex as table
toTable :: (Show a, RealFrac a) => Simplex a -> String
toTable Simplex {basis, nonBasis, table, initialFunc, limited} =
  ( ++
      ( if limited
          then "Maximum = " ++ show result
          else "Not limited"
      )
  )
    . BX.render
    $ BX.hsep
      separator
      BX.top
    $ [BX.vcat BX.center1 ((:) (BX.text (replicate maxSize ' ')) . fmap (\x -> BX.text ("X" ++ show x)) $ basis)]
      ++ fmap column [0 .. (length (head table) - 2)]
      ++ [free]
  where
    column index =
      BX.vcat BX.center1
        . (BX.text ("-X" ++ show (nonBasis !! index)) :)
        . fmap (BX.text . show . roundToDigit digitAfterPoint)
        . (!! index)
        . transpose
        $ table
    free =
      BX.vcat BX.center1
        . (BX.text "B" :)
        . fmap (BX.text . show . roundToDigit digitAfterPoint)
        . last
        . transpose
        $ table
    result =
      roundToDigit digitAfterPoint
        . sum
        . zipWith (\i a -> if i - 1 >= length initialFunc then 0 else initialFunc !! (i - 1) * a) basis
        . init
        . last
        . transpose
        $ table
    maxSize = maximum . fmap (length . show) $ basis
    separator = 5
    digitAfterPoint = 4 :: Int
