-- {-@ LIQUID "--exact-data-con" @-}
{-@ LIQUID "--reflection" @-}

{-@ measure isLeft @-}
{-@ isLeft :: Either a b -> Bool @-}
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

{-@ example :: x:Int -> {v : Either Int String | (isLeft v && x > 0) || (not (isLeft v) && x <= 0)} @-}
example :: Int -> Either Int String
example x =
    if x <= 0 then Right "hello" else Left 1

{-@ measure size @-}
{-@ size :: [a] -> Nat @-}
size :: [a] -> Int
size [] = 0
size (h : t) = 1 + size t

{-@ reflect sameInner @-}
{-@ sameInner :: x:{v : [Int] | size v = 2} -> y:{r : [Int] | size r = 2} -> Maybe [Int] @-}
sameInner :: [Int] -> [Int] -> Maybe [Int]
sameInner ([h1, t1]) ([h2, t2]) = if t1 == h2 then Just [h1, t2] else Nothing
