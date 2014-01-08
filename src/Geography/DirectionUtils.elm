module Geography.DirectionUtils where

import open Geography.Area
import open Geography.Direction
import open Utils.MaybeMonad

asCoords : Direction -> Maybe(Coords)
asCoords dir = case dir of
                    N   ->  Just (coords (-1) 0)
                    NE  ->  Just (coords (-1) 1)
                    E   ->  Just (coords 0 1)
                    SE  ->  Just (coords 1 1)
                    S   ->  Just (coords 1 0)
                    SW  ->  Just (coords 1 (-1))
                    W   ->  Just (coords 0 (-1))
                    NW  ->  Just (coords (-1) (-1))
                    _     ->  Nothing

addDir : Coords->Direction->Maybe(Coords)
addDir a dir = case (asCoords dir) of
                  Just b -> Just (addCoord a b)
                  Nothing -> Nothing

type DirectionalSignal = { from:Coords
                         , targetDir:Direction
                         }

toLocSig : DirectionalSignal -> Maybe (LocationSignal)
toLocSig dSig = let 
                    from = dSig.from
                    to = from `addDir` (dSig.targetDir)
                    locationSignal' to = return (locationSignal from to)
                 in
                    to >>= locationSignal'