module AntColony.Geography.DirectionUtils where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction
import open AntColony.Utils.MaybeMonad

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