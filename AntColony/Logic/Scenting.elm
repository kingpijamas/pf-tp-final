module AntColony.Logic.Scenting where

import open AntColony.Model.Terrain
import open AntColony.Geography.Coords
import open AntColony.Geography.Coords
import open AntColony.Geography.Coords

import AntColony.Model.Scent as Sc
import open AntColony.Utils.Maybe


scent : Terrain -> Coords -> Maybe(Terrain)
scent = scentUnscent (return . Sc.scent)

unscent : Terrain -> Coords -> Maybe(Terrain)
unscent = scentUnscent Sc.unscent

scentUnscent : (Position -> Maybe(Position)) -> Terrain -> Coords -> Maybe(Terrain)
scentUnscent scf terrain whr = let updateTerrain pos' = add terrain whr pos'
                                in
                                   (terrain `get` whr)   -- : Maybe(Position)
                                    >>= scf              -- : Position -> Maybe(Position)
                                    >>= updateTerrain    -- : Position -> Maybe(Terrain)