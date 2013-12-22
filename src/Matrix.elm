module Matrix where
import Dict as D
import Maybe

type Position = (Int,Int)

position:Int->Int->Position
position row col = (row,col)

type Matrix v = { elems:D.Dict Position v
                , rows:Int
                , cols:Int
                }

matrix:(D.Dict Position v)->Int->Int->Matrix v
matrix elems rows cols = {elems=elems, rows=rows, cols=cols}

empty = matrix D.empty

isWithinBounds:Position->(Matrix v)->Bool
isWithinBounds (row,col) {elems,rows,cols} = let isBtwn x (lb,ub) = x>=lb && x<=ub
                                             in
                                                (row `isBtwn` (0,rows)) && (col `isBtwn` (0,cols))

add:(Matrix v)->Position->v->Maybe (Matrix v)
add {elems,rows,cols} pos elem = let mat = matrix elems rows cols
                                 in
                                   if pos `isWithinBounds` mat
                                    then Just (matrix (D.insert pos elem elems) rows cols)
                                    else Nothing

get:(Matrix v)->Position->Maybe v
get {elems,rows,cols} pos = D.lookup pos elems

remove:(Matrix v)->Position->Maybe (Matrix v)
remove {elems,rows,cols} pos = let mat = matrix elems rows cols
                               in
                                 if pos `isWithinBounds` mat
                                  then Just (matrix (D.remove pos elems) rows cols)
                                  else Nothing