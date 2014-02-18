module AntColony.Logic.Scenting where

import open AntColony.Model.Terrain
import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import AntColony.Model.Scentable as Scentable
import open AntColony.Utils.Maybe

scentInDirs : Terrain -> Coords -> [Direction] -> Maybe(Terrain)
scentInDirs terrain from dirs = let scentInDir' dir terrain = scentInDir terrain from dir
        
                                    scentStep dir mbterr = mbterr >>= (scentInDir' dir)
                                 in
                                    foldr scentStep (Just terrain) dirs

scentInDir : Terrain -> Coords -> Direction -> Maybe(Terrain)
scentInDir terrain from dir = (from `addDir` dir)   -- : Maybe(Coords)
                               >>= (scent terrain)  -- : Coords -> Maybe(Terrain)

unscentInDir : Terrain -> Coords -> Direction -> Maybe(Terrain)
unscentInDir terrain from dir = (from `addDir` dir)     -- : Maybe(Coords)
                                 >>= (unscent terrain)  -- : Coords -> Maybe(Terrain)

scent : Terrain -> Coords -> Maybe(Terrain)
scent = scentUnscent (return . Scentable.scent)

unscent : Terrain -> Coords -> Maybe(Terrain)
unscent = scentUnscent Scentable.unscent

scentUnscent : (Position -> Maybe(Position)) -> Terrain -> Coords -> Maybe(Terrain)
scentUnscent scf terrain whr = let updateTerrain pos' = add terrain whr pos'
                                in
                                   (terrain `get` whr)   -- : Maybe(Position)
                                    >>= scf              -- : Position -> Maybe(Position)
                                    >>= updateTerrain    -- : Position -> Maybe(Terrain)

