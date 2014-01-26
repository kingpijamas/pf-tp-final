module AntColony.Model.Moving where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction



import open AntColony.Utils.MaybeMonad

import open AntColony.Model.Data.Terrain

move : Terrain -> Coords -> Coords -> Maybe (Terrain)
move terrain from to = let occupy' (terrain', mvr) = occupy terrain' to mvr
                        in
                          (terrain `evict` from)     -- : Maybe (Terrain, Position)
                           >>= (occupy')             -- : (Terrain, Position) -> Maybe(Terrain)

{-- TODO: it isn't changing the ant's position! --}
occupy : Terrain -> Coords -> Position -> Maybe (Terrain)
occupy terrain whr occ = let occupyPos pos = case pos.occupant of
                                                  Nothing -> return { pos | occupant <- occ }
                                                  _ -> Nothing

                             updateTerrain pos' = add terrain whr pos'
                          in
                             (terrain `get` whr)     -- : Maybe (Position)
                              >>= (occupyPos)        -- : Position -> Maybe(Position)
                              >>= (updateTerrain)    -- : Position -> Maybe(Terrain)

evict : Terrain -> Coords -> Maybe (Terrain, Position)
evict terrain from = let evict pos = case pos.occupant of
                                         Just occ -> return { pos | occupant <- occ }
                                         _   ->  Nothing

                         updateTerrain pos' = return (remove terrain from pos', pos')
                        
                         flatten (mbterr',pos') = case (mbterr') of
                                                       Just terrain' -> return (terrain', pos')
                                                       _   -> Nothing
                      in
                         (terrain `get` from)        -- : Maybe(Position)
                          >>= (evict)                -- : Position -> Maybe(Position)
                          >>= (updateTerrain)        -- : Position -> Maybe(Maybe(Terrain), Position)
                          >>= (flatten)              -- : (Maybe(Terrain),Position) -> Maybe(Terrain, Position)