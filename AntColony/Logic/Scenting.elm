module AntColony.Logic.Scenting where

import open AntColony.Model.Terrain
import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import AntColony.Model.Scent as Scent
import open AntColony.Utils.Maybe

scentInDir : Terrain -> Coords -> Direction -> Maybe(Terrain)
scentInDir terrain from dir = (from `addDir` dir)   -- : Maybe(Coords)
                               >>= (scent terrain)  -- : Coords -> Maybe(Terrain)

unscentInDir : Terrain -> Coords -> Direction -> Maybe(Terrain)
unscentInDir terrain from dir = (from `addDir` dir)     -- : Maybe(Coords)
                                 >>= (unscent terrain)  -- : Coords -> Maybe(Terrain)

scent : Terrain -> Coords -> Maybe(Terrain)
scent = scentUnscent (return . Scent.scent)

unscent : Terrain -> Coords -> Maybe(Terrain)
unscent = scentUnscent Scent.unscent


scentUnscent : (Position -> Maybe(Position)) -> Terrain -> Coords -> Maybe(Terrain)
scentUnscent scf terrain whr = let updateTerrain pos' = add terrain whr pos'
                                in
                                   (terrain `get` whr)   -- : Maybe(Position)
                                    >>= scf              -- : Position -> Maybe(Position)
                                    >>= updateTerrain    -- : Position -> Maybe(Terrain)

