module AntColony.Logic.Scenting where

import open AntColony.Model.Terrain
import open AntColony.Geography.Coords

import AntColony.Model.Scent as Scent
import open AntColony.Utils.Maybe

scentUnscent : (Position -> Maybe(Position)) -> Terrain -> Coords -> Maybe(Terrain)
scentUnscent scf terrain whr = let updateTerrain pos' = add terrain whr pos'
                                in
                                   (terrain `get` whr)   -- : Maybe(Position)
                                    >>= scf              -- : Position -> Maybe(Position)
                                    >>= updateTerrain    -- : Position -> Maybe(Terrain)

scent : Terrain -> Coords -> Maybe(Terrain)
scent = scentUnscent (return . Scent.scent)

unscent : Terrain -> Coords -> Maybe(Terrain)
unscent = scentUnscent Scent.unscent
