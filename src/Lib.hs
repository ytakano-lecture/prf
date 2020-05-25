module Lib
    (
        Nat(Z, S),
        proj,
        compose,
        recurse,
        toNat,
        toInt,
        toBool
    ) where

data Nat =
    Z |   -- 0
    S Nat -- successor
    deriving (Show)

-- projection
proj :: Int -> [Nat] -> [Nat]
proj n args = [args !! n]

-- composition
compose :: ([Nat] -> [Nat]) -> [[Nat] -> [Nat]] -> [Nat] -> [Nat]
compose f gs args = f $ concat [g args | g <- gs]

-- primitive recursive
recurse :: ([Nat] -> [Nat]) -> ([Nat] -> [Nat]) -> [Nat] -> [Nat]
recurse f g (Z : args) = f args
recurse f g (S n : args) = g $ n : (h ++ args)
    where
        h = recurse f g (n : args)

toNat :: Int -> Nat
toNat num = toNat' num Z

toNat' 0 nat   = nat
toNat' num nat = S (toNat' (num - 1) nat)

toInt :: Nat -> Int
toInt nat = toInt' nat 0

toInt' Z num     = num
toInt' (S n) num = toInt' n (num + 1)

toBool :: Nat -> Bool
toBool Z = True
toBool _ = False
