module Model.Ant.Loading where

import open Utils.Tuple
import open Utils.MaybeMonad
import open Model.Terrain
import Capacities.Loading as Ld
import open Model.Food

type LoadF = Ld.LoadF Tile Food



--loadFood : (FoodCarrier a) -> Food -> Maybe (FoodCarrier a, Maybe(Food))

--(ant `loadFood` fd) : Maybe (FoodCarrier a, Maybe(Food))
--                    : (FoodCarrier a, Maybe(Food)) -> 
--                    :

--(.) : (b -> c) -> (a -> b) -> (a -> c)
--return : a -> Maybe a
--======================================
--b : a
--c : Maybe a

--((.) return) : (d -> a) -> (d -> Maybe a)

--mapFst : (a -> b) -> (a, c) -> (b, c)
--asAnt : AntT -> Occupant
--======================================
--a : AntT
--b : Occupant

--(mapFst asAnt) : (AntT, c) -> (Occupant, c)

--((.) return) : (d -> a) -> (d -> Maybe a)
--(mapFst asAnt) : (AntT, c) -> (Occupant, c)
--======================================
--d : (AntT, c)
--a : (Occupant, c)

--((.) return (mapFst asAnt)) : (AntT, c) -> Maybe (Occupant, c)
                            --load' : (AntT, c) -> Maybe (Occupant, c)

                                


load : LoadF    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
load terrain ldrPos fd = let --setOccupant' : Tile -> (Occupant, Maybe(Food)) -> Maybe(Tile, Maybe(Food))

                             setOccupant' tile (occ',rem) = return (setOccupant tile (return occ'),rem)  -- : Tile -> (Occupant, Maybe(Food)) -> Maybe(Tile, Maybe(Food))

                             loadOcc tile = case tile.occupant of
                                                Just(AntT ant) -> (ant `loadFood` fd)                   -- : Maybe(AntT, Maybe(Food))
                                                                    >>= (return . (mapFst asAnt))       -- : (AntT, Maybe(Food)) -> Maybe(Occupant, Maybe(Food))
                                                                    >>= (setOccupant' tile)             -- : (Occupant, Maybe(Food)) -> Maybe(Tile, Maybe(Food))
                                                Just(AntNestT nest) -> (nest `loadFood` fd)             -- : Maybe(AntNestT, c)
                                                                        >>= (return . (mapFst asNest))  -- : (AntT, c) -> Maybe(Occupant, Maybe(Food))
                                                                        >>= (setOccupant' tile)         -- : (Occupant, Maybe(Food)) -> Maybe(Tile, Maybe(Food))
                                                _ -> Nothing

                             updateTerrain (tile',rem) = return (terrain `add` tile', rem)
                          in 
                             (terrain `get` ldrPos)                    -- : Maybe(Tile)
                              >>= (loadOcc)                            -- : Tile -> Maybe(Tile, Maybe(Food))
                              >>= (updateTerrain)                      -- : (Tile, Maybe(Food)) -> Maybe(Maybe(Terrain),Maybe(Food))
                              >>= (joinFst)                            -- : (Maybe(Terrain),Maybe(Food)) -> Maybe(Terrain, Maybe(Food))


--type UnloadF = Ld.UnloadF Tile FoodT

--unload : UnloadF    -- : Terrain -> Coords -> Maybe(Terrain, Food)
--unload terrain unldrPos = let unload' occ = case occ of
--                                                Ant ant -> (Fd.unload ant) >>= (return . (mapFst asAnt)) 
--                                                AntNest nest -> (Fd.unload nest) >>= (return . (mapFst asAnt))
--                                                _ -> Nothing

--                              updateTerrain tile' = terrain `add` tile'
--                           in
--                              (terrain `get` unldrPos)      -- : Maybe(Tile)
--                               >>= (.occupant)              -- : Tile -> Maybe(Occupant)
--                               >>= (unload')                -- : Occupant -> Maybe(Tile, Food)
--                               >>= (mapFst updateTerrain)   -- : (Tile, Food) -> Maybe(Maybe(Terrain), Food)
--                               >>= (joinFst)                -- : (Maybe(Terrain), Food) -> Maybe(Terrain, Food)

--type Loader = Ld.Loader Tile

--loader : Terrain -> Loader
--loader terrain = Ld.loader load unload terrain