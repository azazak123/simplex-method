# Simplex method

Simplex method for solving a problems implemented on Haskell.

## Basic usage

``` haskell
-- solve problem with these constraunts:
-- 
-- 3x1 + x2 + x3 + x4 + x5 = 5
-- 2x1 - x2 + 3x3 = 4
-- 5x2 + 6x3 + x4 = 11
--
-- f(x1, x2, x3, x4, x5) = 5x1 - x2 + x3
--
simplex = simplexMethod [[3, 1, 1, 1, 1], [2, -1, 3, 0, 0], [0, 5, 6, 1, 0]]
                        ["=", "=", "="] 
                        [5, 4, 11] 
                        [5, -1, 1, 0, 0]

-- after solving, we can obtain solutions using getSolution:
getSolution simplex -- [1.0,1.0,1.0]

-- or we can render and print table:
putStrLn . toTable $ simplex

--          -X6         -X8          -X7         -X4          -X5       free
-- X1      0.3231     -6.15e-2     1.54e-2      0.2615       0.3231     1.0
-- X3     -0.1538     7.69e-2       0.2308     -7.69e-2     -0.1538     1.0
-- X2      0.1846      0.1077      -0.2769      0.2923       0.1846     1.0
--          1.0         1.0          1.0         0.0          0.0       0.0
-- Maximum = 5.0
```

## Documentation 

To generate documentation run

``` bash
cabal haddock
```

## Test

To run test run

```bash
cabal test
```
