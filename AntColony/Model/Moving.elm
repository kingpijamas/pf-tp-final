module AntColony.Model.Moving where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Capacities.Positioning

import open AntColony.Utils.MaybeMonad
import open AntColony.Utils.SignalFunction

import open AntColony.Model.Data.Terrain

mv : Terrain -> LocationIntent -> Maybe (Terrain)
mv area sig = let from = sig.from
                  to = sig.target
       
                  occupyWith (area', mver) = occupy area' to mver
               in
                  (area `evict` from)               -- : Maybe (Terrain, Tile)
                    >>= (occupyWith)                -- : (Terrain, Tile) -> Maybe(Terrain)

{-- TODO: it isn't changing the ant's position! --}
occupy : Terrain -> Coords -> Tile -> Maybe (Terrain)
occupy terrain pos occ = let occupyTile tile = case tile.occupant of
                                                    Nothing -> return { tile | occupant <- occ }
                                                    _ -> Nothing

                             updateTerrain tile' = add terrain pos tile'
                          in
                            (terrain `get` pos)     -- : Maybe (Tile)
                             >>= (occupyTile)       -- : Tile -> Maybe(Tile)
                             >>= (updateTerrain)    -- : Tile -> Maybe(Terrain)

evict : Terrain -> Coords -> Maybe (Terrain, Tile)
evict terrain pos = let evictTile tile = case tile.occupant of
                                            Just occ -> return { tile | occupant <- occ }
                                            _   ->  Nothing

                        updateTerrain tile' = return (remove terrain pos tile', tile')
                        
                        flatten (mbarea',tile') = case (mbarea') of
                                                    Just area' -> return (area', tile')
                                                    _   -> Nothing
                     in
                       (terrain `get` pos)         -- : Maybe(Tile)
                        >>= (evictTile)            -- : Tile -> Maybe(Tile)
                        >>= (updateTerrain)        -- : Tile -> Maybe(Maybe(Terrain), Tile)
                        >>= (flatten)              -- : (Maybe(Terrain),Tile) -> Maybe(Terrain, Tile)

--type Motor = Mv.Motor Tile -- : SF (DirectionIntent) (Maybe(Terrain))

--motor : Terrain -> Motor
--motor area = Mv.motor occupy evict area


--type Motor a = SF (DirectionIntent) (Maybe(Area a))

--motor : (OccupiationF a) -> (EvictionF a) -> (Area a) -> (Motor a)
--motor occupy evict area = arr(toLocSig) >>> impure(mv occupy evict area)