module AntColony.Model.Scenting where

import open AntColony.Model.Data.Terrain
import open AntColony.Geography.Area

import AntColony.Model.Data.Scentable as Sc
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