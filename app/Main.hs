module Main where

import           Lib

pred' Z     = Z
pred' (S n) = n

-- addition
plus Z y     = y
plus (S n) y = S (plus n y)

-- substraction
monus x Z     = x
monus x (S n) = pred' (monus x n)

true' = Z

-- if expression
if' Z x _ = x
if' _ _ y = y

-- euqality
equal x y = plus n1 n2
    where
        n1 = monus x y
        n2 = monus y x

-- greater than or equal to
-- x >= y
gt x y = if' e true' m
    where
        e = equal x y
        m = monus y x

main :: IO ()
main = do
    -- example of addition
    let n1 = toNat 10
        n2 = toNat 20
        n  = plus n1 n2
    print $ toInt n

    -- example of substraction
    let n1 = toNat 43
        n2 = toNat 27
        n  = monus n1 n2
    print $ toInt n

    -- example of >=
    let n1 = toNat 7
        n2 = toNat 4
        n  = gt n1 n2
    print $ toBool n

    pure ()
