module Utils.Tuple where
import open Utils.MaybeMonad

mapFst : (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

joinFst : (Maybe a, b) -> Maybe (a, b)
joinFst (mbx, y) = mbx >>= (\x -> (x,y))