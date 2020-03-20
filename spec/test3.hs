-- {-@ type Nat = {v : Int | v >= 0} @-}

{-@ assume okay :: v : Int -> {u : Int | u = v} @-}
okay :: Int -> Int
okay x = x + 1

{-@ takesNat :: {v : Int | v = 2} -> Bool @-}
takesNat :: Int -> Bool
takesNat 2 = True

shouldBeOkay = takesNat (okay 2)
