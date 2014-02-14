module AntColony.Utils.Tuple where

import open AntColony.Utils.Maybe

mapFst : (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

joinFst : (Maybe a, b) -> (Maybe(a, b))
joinFst (mbx, y) = mbx >>= (\x -> return (x,y))

joinSnd : (a, Maybe b) -> Maybe(a, b)
joinSnd (x, mby) = mby >>= (\y -> return (x,y))

(&) : (a -> b) -> (a -> c) -> a -> (b,c)
f1 & f2 = \x -> (f1 x, f2 x)

flatten : ((a,b), c) -> (a,b,c)
flatten ((x,y),z) = (x,y,z)