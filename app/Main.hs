module Main where

import           Lib

-- helper function for S
succ' [n] = [S n]

pred' [Z]   = [Z]
pred' [S n] = [n]

-- addition
plus [Z, y]   = [y]
plus [S x, y] = succ' $ plus [x, y]

-- addition can be defined as well as following
-- plus = recurse f g
--  where f = proj 0
--        g = compose succ' [proj 1]

-- substraction
monus [x, Z]   = [x]
monus [x, S n] = pred' $ monus [x, n]

true' = Z

-- if expression
if' [Z, x, _] = [x]
if' [_, _, y] = [y]

-- euqality
equal [x, y] = plus [n1, n2]
    where
        [n1] = monus [x, y]
        [n2] = monus [y, x]

-- greater than or equal to
-- x >= y
gt [x, y] = if' [e, true', m]
    where
        [e] = equal [x, y]
        [m] = monus [y, x]

-- implement this function
mod' [x, y] = [x]

-- implement this function
norm [x, y] = [x]

main :: IO ()
main = do
    -- example of addition
    let n1  = toNat 10
        n2  = toNat 20
        [n] = plus [n1, n2]
    putStr $ show (toInt n1) ++ " + " ++ show (toInt n2) ++ " = "
    print $ toInt n

    -- example of substraction
    let n1  = toNat 43
        n2  = toNat 27
        [n] = monus [n1, n2]
    putStr $ show (toInt n1) ++ " - " ++ show (toInt n2) ++ " = "
    print $ toInt n

    -- example of >=
    let n1  = toNat 7
        n2  = toNat 4
        [n] = gt [n1, n2]
    putStr $ show (toInt n1) ++ " >= " ++ show (toInt n2) ++ " = "
    print $ toBool n

    -- modulo
    let n1  = toNat 34
        n2  = toNat 7
        [n] = mod' [n1, n2]
    -- must be same
    putStr $ show (toInt n1) ++ " mod " ++ show (toInt n2) ++ " = "
    print $ toInt n
    putStr $ show (toInt n1) ++ " mod " ++ show (toInt n2) ++ " = "
    print $ toInt n1 `mod` toInt n2 -- must be same

    -- norm
    let n1  = toNat 52
        n2  = toNat 86
        [n] = norm [n1, n2]
    putStr $ "|" ++ show (toInt n1) ++ " - " ++ show (toInt n2) ++ "| = "
    print $ toInt n
    putStr $ "|" ++ show (toInt n1) ++ " - " ++ show (toInt n2) ++ "| = "
    print $ abs $ toInt n1 - toInt n2 -- must be same

    pure ()
