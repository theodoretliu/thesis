{-@ LIQUID "--exact-data-con" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

import Prelude hiding (reverse, max, drop)

{-@ type Nat = {v: Int | v >= 0} @-}

{-@ measure size @-}
{-@ size :: [a] -> Nat @-}
size :: [a] -> Int
size [] = 0
size (hd : tl) = 1 + size tl

data Vector a = V {vLen::Int, elts::[a]} deriving (Eq)
{-@ data Vector a = V {vLen::Nat, elts::{v : [a] | size v == vLen}} @-}
{-@ type VectorN a N = {v : Vector a | vLen v = N} @-}

ok = V 2 [1,2]
-- bad = V 3 [1, 1]

{-@ type ListNE a = {v : [a] | size v > 0} @-}

{-@ measure head1 @-}
{-@ head1 :: ListNE Nat -> Nat @-}
head1 :: [Int] -> Int
head1 (h : t) = h

{-@ measure tail1 @-}
{-@ tail1 :: ListNE Nat -> [Nat] @-}
tail1 :: [Int] -> [Int]
tail1 (h : t) = t

data Nparray a = N {nDims::[Int], nElts::Either a (Vector (Nparray a))}

{-@ measure ords @-}
{-@ ords :: Nparray a -> [Nat] @-}
ords :: Nparray a -> [Int]
ords (N nDims _nElts) = nDims

{-@ measure isLeft @-}
isLeft (Left _) = True
isLeft (Right _) = False

{-@ data Nparray a = N {nDims::[Nat],
                        nElts::{v : Either a (VectorN ({x : Nparray a | (tail1 nDims) == (ords x)}) {head1 nDims})
                                | (isLeft v && size nDims = 0) || (not (isLeft v) && size nDims > 0) }} @-}

{-@ type NparrayN a N = {v : Nparray a | nDims v = N} @-}

ok1 = N [] (Left 2)

okArray = N [2] (Right (V 2 [N [] (Left 1), N [] (Left 2)]))

-- notOk1 = N [] (Right (V 1 [N [] (Left 2)]))
-- notOk2 = N [2] (Right (V 1 [N [] (Left 1), N [] (Left 2)]))
-- notOk3 = N [2, 3] (Right (V 2 [N [3] (Right ())]))

{-@ measure order @-}
{-@ order :: Nparray a -> Nat @-}
order :: Nparray a -> Int
order (N nDims nElts) = size nDims

{-@ reflect sameInner @-}
{-@ sameInner :: {v : [Nat] | size v = 2} -> {r : [Nat] | size r = 2} -> Bool @-}
sameInner :: [Int] -> [Int] -> Bool
sameInner ([h1, t1]) ([h2, t2]) = t1 == h2

{-@ reflect getOuter @-}
{-@ getOuter :: {v : [Nat] | size v = 2} -> {r : [Nat] | size r = 2} -> {s : [Nat] | size s = 2} @-}
getOuter :: [Int] -> [Int] -> [Int]
getOuter ([h1, t1]) ([h2, t2]) = [h1, t2]

{-@ measure extract @-}
{-@ extract :: {v : Maybe a | isJust v} -> a @-}
extract :: Maybe a -> a
extract (Just x) = x

{-@ assume matmul :: {v : Nparray a | order v = 2}
              -> {r : Nparray a | order r = 2 && sameInner (nDims v) (nDims r)}
              -> {s : Nparray a | order s = 2 && nDims s = getOuter (nDims v) (nDims r)} @-}
matmul :: Nparray a -> Nparray a -> Nparray a
matmul (N ndims1 nelts1) (N ndims2 nelts2) = (N ndims1 nelts1)

{-@ reflect canBroadcast @-}
{-@ canBroadcast :: [Nat] -> [Nat] -> Bool @-}
canBroadcast :: [Int] -> [Int] -> Bool
canBroadcast [] [] = True
canBroadcast [] _ = True
canBroadcast _ [] = True
canBroadcast l1 l2 = canBroadcast' (reverse l1) (reverse l2)

{-@ reflect canBroadcast' @-}
{-@ canBroadcast' :: [Nat] -> [Nat] -> Bool @-}
canBroadcast' :: [Int] -> [Int] -> Bool
canBroadcast' (h1:t1) (h2:t2) =
        (h1 == 1 || h2 == 1 || h1 == h2) && canBroadcast' t1 t2
canBroadcast' _ _ = False

{-@ reflect reverse @-}
{-@ reverse :: [a] -> [a] @-}
reverse :: [a] -> [a]
reverse [] = []
reverse (h : t) = (reverse t) ++ [h]

{-@ reflect max @-}
{-@ max :: Nat -> Nat -> Nat @-}
max :: Int -> Int -> Int
max a b = if a > b then a else b

{-@ reflect broadcast @-}
{-@ broadcast :: [Nat] -> [Nat] -> [Nat] @-}
broadcast :: [Int] -> [Int] -> [Int]
broadcast [] [] = []
broadcast x [] = x
broadcast [] x = x
broadcast l1 l2 = reverse (broadcast' l1 l2)

{-@ reflect broadcast' @-}
{-@ broadcast' :: [Nat] -> [Nat] -> [Nat] @-}
broadcast' :: [Int] -> [Int] -> [Int]
broadcast' (h1:t1) (h2:t2) = (max h1 h2) : (broadcast' t1 t2)
broadcast' _ _ = []

{-@ type NparrayBinop = {v : Nparray a | true}
                        -> {w: Nparray a | canBroadcast (nDims v) (nDims w)}
                        -> {x: Nparray a | nDims x = broadcast (nDims v) (nDims w)} @-}

{-@ assume plus :: NparrayBinop @-}
plus :: Nparray a -> Nparray a -> Nparray a
plus a1 a2 = a1

{-@ reflect dropList' @-}
{-@ dropList' :: [Nat] -> [Nat] -> Nat -> [Nat] @-}
dropList' :: [Int] -> [Int] -> Int -> [Int]
dropList' (h:t) (dim:remainDims) count =
        let rest = dropList' t remainDims (count + 1) in
        if h == count then rest
        else dim : rest
dropList' _ l _count = l

{-@ reflect dropList @-}
{-@ dropList :: [Nat] -> [Nat] -> [Nat] @-}
dropList :: [Int] -> [Int] -> [Int]
dropList [] dims = dims
dropList toDrop [] = []
dropList toDrop dims = dropList' toDrop dims 0

{-@ type NparrayReduce = v : Nparray a
                         -> u : [Nat]
                         -> {t : Nparray a | nDims t = dropList u (nDims v)} @-}

{-@ assume sum :: NparrayReduce @-}
sum :: Nparray a -> [Int] -> Nparray a
sum a l = a
