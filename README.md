## Setting up Haskell Environment

### Linux

Install stack from the web site.
```
$ wget -qO- https://get.haskellstack.org/ | sh
```

### Mac

Use homebrew to install stack.
```
$ brew install stack
```

## Fetching Source

```
$ git clone https://github.com/ytakano-lecture/prf.git
$ cd prf
```

## Compiling and Executing

```
$ stack build
$ stack run
```

## Functions

### Primitive and Partial Recursive Functions

Primitive and partial recursive functions are defined in src/Lib.hs.

```haskell:src/Lib.hs
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
```

### Arithmetic Functions

Arithmetic functions can be defined by using primitive
or partial recursive functions as follows.
Some arithmetic functions are defined in app/Main.hs.

```haskell:src/Lib.hs
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
```

### Printing

You can print results by using print function.
See main function in app/Main.hs.

```
print $ toInt n
```