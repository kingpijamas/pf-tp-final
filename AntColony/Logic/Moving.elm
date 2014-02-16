module AntColony.Logic.Moving where

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import open AntColony.Model.Terrain
import open AntColony.Model.AntT

import open AntColony.Utils.Maybe
import open AntColony.Utils.Tuple


moveInDir : Terrain -> Coords -> Direction -> Maybe(Terrain)
moveInDir terrain from dir = (from `addDir` dir)      -- : Maybe(Coords)
                              >>= (move terrain from) -- : Coords -> Maybe(Terrain)


move : Terrain -> Coords -> Coords -> Maybe(Terrain)
move terrain from to = let occupy' (terrain', mvr) = occupy terrain' to mvr
                        in
                           (terrain `evict` from)     -- : Maybe (Terrain, Occupant)
--                           >>= (return . fst)          --TODO: TEST!
                            >>= (occupy')           -- : (Terrain, Occupant) -> Maybe(Terrain)


evict : Terrain -> Coords -> Maybe(Terrain, Occupant)
evict terrain from = let evict' pos = case pos.occupant of
                                           Just occ -> return (empty pos, occ)
                                           _   ->  Nothing

                         updateTerrain (pos', occ) = return (add terrain from pos', occ)
                      in
                         (terrain `get` from)        -- : Maybe(Position)
                          >>= (evict')               -- : Position -> Maybe(Position, Occupant)
                          >>= (updateTerrain)        -- : Position -> Maybe(Maybe(Terrain), Occupant)
                          >>= (joinFst)              -- : (Maybe(Terrain), Occupant) -> Maybe(Terrain, Occupant)


occupy : Terrain -> Coords -> Occupant -> Maybe(Terrain)
occupy terrain whr occ = let occupy' pos = case (pos.occupant, occ) of
                                                (Nothing, Ant ant) -> return (pos `setOccupant2` (asAnt (ant `setPosition` whr)))
                                                _ -> Nothing

                             updateTerrain pos' = add terrain whr pos'
                          in
                             (terrain `get` whr)     -- : Maybe(Position)
                              >>= (occupy')          -- : Position -> Maybe(Position)
                              >>= (updateTerrain)    -- : Position -> Maybe(Terrain)

