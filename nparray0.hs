{-@ LIQUID "--exact-data-con" @-}
data Nparray a = Const a | Array [Nparray a]

{-@ measure len @-}
len :: [a] -> Int
len [] = 0
len (hd : tl) = 1 + len tl

{-@ measure shape @-}
shape :: Nparray a -> [Int]
shape (Const a) = []
shape (Array []) = [0]
shape (Array (hd : tl)) = (len (hd : tl)) : (shape hd)

{-@ type NparrayN a S = {v: Nparray a | shape v == S} @-}

{-@ example :: {v : Nparray _ | shape v == [2]} @-}
example = Array [Const 1, Const 2]

{-@ inline list22 @-}
list22 :: [Int]
list22 = [2, 2]

{-@ example2 :: NparrayN _ list22 @-}
example2 = Array [Array [Const 1, Const 2], Array [Const 3, Const 4]]

{-@ matmul :: {v : NparrayN _ S | len S == 2} @-}
matmul = Array [ Array [Const 1] ]
