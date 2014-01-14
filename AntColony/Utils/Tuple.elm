module AntColony.Utils.Tuple where
import open AntColony.Utils.MaybeMonad

mapFst : (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

joinFst : (Maybe a, b) -> Maybe (a, b)
joinFst (mbx, y) = mbx >>= (\x -> return (x,y))

joinSnd = flip (curry joinFst)