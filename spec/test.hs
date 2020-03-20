-- {-@ LIQUID "--exact-data-con" @-}
-- {-@ LIQUID "--ple" @-}

{-@ measure isLeft :: EitherIntString -> Bool @-}
isLeft (L _) = True
isLeft (R _) = False

{-@ asdf :: x:Int -> {v : EitherIntString | ((isLeft v && x > 0) || (not (isLeft v) && x <= 0))} @-}
asdf x =
    if x <= 0 then R "hello" else L 1

{-@ measure sameInner @-}
{-@ sameInner :: {v : [Int] | size v = 2} -> {r : [Int] | size r = 2} -> Maybe [Int] @-}
sameInner :: [Int] -> [Int] -> Maybe [Int]
sameInner ([h1, t1]) ([h2, t2]) = if t1 == h2 then Just [h1, t2] else Nothing
