module AntColony.Model.Moving where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Capacities.Positioning

import open AntColony.Utils.MaybeMonad

import open AntColony.Model.Data.Terrain

mv : Terrain -> LocationIntent -> Maybe (Terrain)
mv terrain sig = let from = sig.from
                     to = sig.target
       
                     occupyWith (terrain', mver) = occupy terrain' to mver
                  in
                     (terrain `evict` from)          -- : Maybe (Terrain, Position)
                       >>= (occupyWith)              -- : (Terrain, Position) -> Maybe(Terrain)

{-- TODO: it isn't changing the ant's position! --}
occupy : Terrain -> Coords -> Position -> Maybe (Terrain)
occupy terrain pos occ = let occupyPos pos = case pos.occupant of
                                                  Nothing -> return { pos | occupant <- occ }
                                                  _ -> Nothing

                             updateTerrain pos' = add terrain pos pos'
                          in
                             (terrain `get` pos)     -- : Maybe (Position)
                              >>= (occupyPos)        -- : Position -> Maybe(Position)
                              >>= (updateTerrain)    -- : Position -> Maybe(Terrain)

evict : Terrain -> Coords -> Maybe (Terrain, Position)
evict terrain pos = let evictPos pos = case pos.occupant of
                                            Just occ -> return { pos | occupant <- occ }
                                            _   ->  Nothing

                        updateTerrain pos' = return (remove terrain pos pos', pos')
                        
                        flatten (mbterrain',pos') = case (mbterrain') of
                                                         Just terrain' -> return (terrain', pos')
                                                         _   -> Nothing
                     in
                       (terrain `get` pos)         -- : Maybe(Position)
                        >>= (evictPos)             -- : Position -> Maybe(Position)
                        >>= (updateTerrain)        -- : Position -> Maybe(Maybe(Terrain), Position)
                        >>= (flatten)              -- : (Maybe(Terrain),Position) -> Maybe(Terrain, Position)

--type Motor = Mv.Motor Position -- : SF (DirectionIntent) (Maybe(Terrain))

--motor : Terrain -> Motor
--motor terrain = Mv.motor occupy evict terrain


--type Motor a = SF (DirectionIntent) (Maybe(Area a))

--motor : (OccupiationF a) -> (EvictionF a) -> (Area a) -> (Motor a)
--motor occupy evict terrain = arr(toLocSig) >>> impure(mv occupy evict terrain)