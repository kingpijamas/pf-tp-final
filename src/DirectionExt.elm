module DirectionExt where
import Area as A
import Directions as D
import MaybeMonad as M

(>>=) = (>>=)  {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

asCoords:D.Direction -> Maybe(A.Coords)
asCoords dir = case dir of
                    D.N   ->  Just (A.coords (-1) 0)
                    D.NE  ->  Just (A.coords (-1) 1)
                    D.E   ->  Just (A.coords 0 1)
                    D.SE  ->  Just (A.coords 1 1)
                    D.S   ->  Just (A.coords 1 0)
                    D.SW  ->  Just (A.coords 1 (-1))
                    D.W   ->  Just (A.coords 0 (-1))
                    D.NW  ->  Just (A.coords (-1) (-1))
                    _     ->  Nothing

addDir:A.Coords->D.Direction->Maybe(A.Coords)
addDir a dir = case (asCoords dir) of
                  Just b -> Just (A.addCoord a b)
                  Nothing -> Nothing

type DirectionalSignal a = { who:A.Locatable a
                           , targetDir:D.Direction
                           }

toLocSig : DirectionalSignal a -> Maybe (A.LocationSignal a)
toLocSig dSig = let 
                    who = dSig.who
                    to = (who.location) `addDir` (dSig.targetDir)
                    locationSignal' to = M.return (A.locationSignal who to)
                 in
                    to >>= locationSignal'