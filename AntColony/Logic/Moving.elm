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
move terrain from to = let occupy' (terrain', occ) = occupy terrain' to occ
                        in
                           (terrain `evict` from)     -- : Maybe (Terrain, Occupant)
                            >>= (\(terrain',occ)->return terrain')          --TODO: TEST!
                           -- >>= (occupy')           -- : (Terrain, Occupant) -> Maybe(Terrain)


evict : Terrain -> Coords -> Maybe(Terrain, Occupant)
evict terrain from = let evict' pos = case pos.occupant of
                                           --_ -> return (empty pos, Rock)
                                           Just occ -> return (empty pos, occ)
                                           _   ->  Nothing

                         updateTerrain (pos', occ) = return (add terrain from pos', occ)

                         f (mbterrain, occ) = case mbterrain of
                                                   Just t -> return (t, Rock)
                                                   Nothing -> Nothing

                      in
                         (terrain `get` from)        -- : Maybe(Position)
                          -- >>= (\_ -> return (terrain,Rock))                          
                          >>= (evict')               -- : Position -> Maybe(Position, Occupant)
                          -- >>= (\_-> return (terrain, Rock))
                          >>= (updateTerrain)        -- : Position -> Maybe(Maybe(Terrain), Occupant)
                          -- >>= (\_ -> return (terrain, Rock))
                          >>= (f)
                          -- >>= (joinFst)              -- : (Maybe(Terrain), Occupant) -> Maybe(Terrain, Occupant)


occupy : Terrain -> Coords -> Occupant -> Maybe(Terrain)
occupy terrain whr occ = let occupy' pos = case (pos.occupant, occ) of
                                                (Nothing, Ant ant) -> return (pos `setOccupant2` (asAnt (ant `setPosition` whr)))
                                                _ -> Nothing

                             updateTerrain pos' = add terrain whr pos'
                          in
                             (terrain `get` whr)     -- : Maybe(Position)
                              -->>= (\pos-> return terrain)    -- : TODO
                               >>= (occupy')          -- : Position -> Maybe(Position)
                               >>= (updateTerrain)    -- : Position -> Maybe(Terrain)

