{-# LANGUAGE NamedFieldPuns #-}

module Math.SimplexMethod (step, simplexMethod, toTable, getSolution) where

import Data.Foldable (find)
import Data.List (transpose)
import Data.Maybe (fromJust, isNothing)
import Math.SimplexMethod.Utils
import qualified Text.PrettyPrint.Boxes as BX

type ConstraintCoefficients a = [[a]]

type Symbols = [String]

type FreeElements a = [a]

type FunctionCoefficients a = [a]

-- | Start simplex method
--
-- Example:
--
-- > simplexMethod [[5, -2],[-1, 2],[1,1]] ["<=","<=","\>="] [7,5,6] [1,2]
-- means:
--
-- 5x1 - 2x2 <= 7
--
-- \-x1 + 2x2 <= 5
--
-- x1 + x2 >= 6
--
--
-- f(x1,x2) = x1 + 2x2
simplexMethod ::
  (RealFrac a, Enum a) =>
  ConstraintCoefficients a ->
  Symbols ->
  FreeElements a ->
  FunctionCoefficients a ->
  Simplex a
simplexMethod coeffs symbols free func = loop initial
  where
    initial =
      Simplex
        { basis = [basisStart + 1 .. basisStart + length coeffs],
          nonBasis = [1 .. basisStart],
          table,
          initialFunc = func,
          limited = True
        }

    table =
      transpose (transpose coeffs ++ greaterColumns ++ [free])
        ++ [fmap negate newC]

    newC =
      foldl1 (zipWith (+)) coeffs
        ++ replicate greaterNumber (-1)
        ++ [sum free]

    greaterColumns =
      fmap (($ replicate (length coeffs) 0) . (`replace` (-1)) . fst)
        . filter ((== ">=") . snd)
        . zip [0 ..]
        $ symbols

    basisStart = length func + greaterNumber
    greaterNumber = length . filter (== ">=") $ symbols

-- | Get solution from Simplex
getSolution :: RealFrac a => Simplex a -> [a]
getSolution Simplex {basis, table, initialFunc, limited}
  | not limited = []
  | otherwise =
      defaultRound
        . (solutions !!)
        . subtract 1
        . fromJust
        . (`find` basis)
        . (==)
        . (+ 1)
        <$> [0 .. subtract 1 . length . filter (/= 0) $ initialFunc]
  where
    solutions = init . last . transpose $ table

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
    initialFunc :: ![a],
    limited :: Bool
  }
  deriving (Eq, Show)

-- | Simplex method main action
step :: RealFrac a => Simplex a -> Simplex a
step initial@Simplex {basis, nonBasis, table, initialFunc}
  | all (>= 0) free && all (>= 0) func && isMinusColumn = initial
  | isNothing indexes = initial {limited = False}
  | otherwise =
      Simplex
        { basis = newBasis,
          nonBasis = newNonBasis,
          table = newTable,
          initialFunc,
          limited = True
        }
  where
    isMinusColumn =
      and
        . zipWith
          ((<=) . (== 0) . defaultRound)
          func
        . fmap (any (> 0) . fmap defaultRound)
        . transpose
        $ table

    newTable =
      transpose
        . replace x pivotColumn
        . transpose
        . replace y pivotRow
        $ others

    pivotRow = replace x (1 / pivot) . fmap (/ pivot) $ table !! y
    pivotColumn =
      replace y (1 / pivot)
        . fmap (negate . (/ pivot))
        $ transpose table !! x

    others = fmap deterY [0 .. length table - 1]
    deterY y1 = fmap deterX [0 .. length (head table) - 1]
      where
        deterX x1 =
          determinant
            [ [table !! y1 !! x1, table !! y1 !! x],
              [table !! y !! x1, pivot]
            ]
            / pivot

    indexes = getPivotIndexes table free
    x = snd $ fromJust indexes
    y = fst $ fromJust indexes
    pivot = table !! y !! x

    newBasis = replace y (nonBasis !! x) basis
    newNonBasis = replace x (basis !! y) nonBasis

    free = last $ transpose table
    func = last table

-- | Find pivot element indexes
getPivotIndexes :: RealFrac a => [[a]] -> [a] -> Maybe (Int, Int)
getPivotIndexes table free
  | isMinusFree = Just (getY id x1, x1)
  | isMinusFunc = Just (getY (filter ((> 0) . snd)) x2, x2)
  | otherwise = Nothing
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
        . zipWith (\coeffs (i, b) -> (coeffs / b, i)) free
        . f
        . zip [0 :: Int ..]
        . init
        $ transpose table !! xarg

    isMinusFree = or . fmap checkMinusFree $ init table
    isMinusFunc = or . fmap checkMinusFunc . init $ transpose table

-- | Check that if free element < 0 then exist any element which < 0
checkMinusFree :: RealFrac a => [a] -> Bool
checkMinusFree = checkMinus (< 0)

-- | Check that if func element < 0 then exist any element which > 0
checkMinusFunc :: RealFrac a => [a] -> Bool
checkMinusFunc = checkMinus (> 0)

checkMinus :: RealFrac a => (a -> Bool) -> [a] -> Bool
checkMinus f arr =
  defaultRound (last arr) < 0
    && any
      (f . defaultRound)
      (init arr)

-- | Render Simplex as table
toTable :: (Show a, RealFrac a) => Simplex a -> String
toTable Simplex {basis, nonBasis, table, initialFunc, limited} =
  ( ++
      if limited
        then "Maximum = " ++ show result ++ "\n"
        else "Not limited" ++ "\n"
  )
    . BX.render
    $ BX.hsep
      separator
      BX.top
    $ BX.vcat
      BX.center1
      (wasteElement : (BX.text . ("X" ++) . show <$> basis))
      : fmap column [0 .. (length (head table) - 2)]
      ++ [free]
  where
    column index =
      BX.vcat BX.center1
        . (BX.text ("-X" ++ show (nonBasis !! index)) :)
        . fmap (BX.text . show . defaultPrintRound)
        . (!! index)
        . transpose
        $ table

    free =
      BX.vcat BX.center1
        . (BX.text "free" :)
        . fmap (BX.text . show . defaultPrintRound)
        . last
        . transpose
        $ table

    result =
      defaultPrintRound
        . sum
        . zipWith
          ( \i a ->
              if i - 1 >= length initialFunc
                then 0
                else initialFunc !! (i - 1) * a
          )
          basis
        . init
        . last
        . transpose
        $ table

    wasteElement =
      BX.text
        (maximum (fmap (length . show) basis) `replicate` ' ')

    separator = 5
    defaultPrintRound = roundToDigit (4 :: Int)
