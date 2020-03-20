data Matrix a = M { numRows :: Int, numCols :: Int }
{-@ matmul :: forall A B C :: Int, {v : Matrix a | numRows = A && numCols = B}
                                 -> {r : Matrix a | numRows = B && numCols = C}
                                 -> {s : Matrix | numRows = A && numCols = C} @-}
