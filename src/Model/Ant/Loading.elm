module Model.Ant.Loading where

import open Utils.Tuple
import open Utils.MaybeMonad
import open Model.Terrain
import Capacities.Loading as Ld
import Model.Food as Fd

type LoadF = Ld.LoadF Tile FoodT

load : LoadF    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
load terrain ldrPos fd = let load' occ = case occ of
                                            Ant ant -> (Fd.load ant fd) >>= (return . (mapFst asAnt))
                                            AntNest nest -> (Fd.load nest fd) >>= (return . (mapFst asNest))
                                            _ -> Nothing

                             updateTerrain tile' = terrain `add` tile'

                          in 
                             (terrain `get` ldrPos)         -- : Maybe(Tile)
                              >>= (.occupant)               -- : Tile -> Maybe(Occupant)
                              >>= (load')                   -- : Occupant -> Maybe(Tile, Maybe(Food))
                              >>= (mapFst updateTerrain)    -- : (Tile, Maybe(Food)) -> Maybe(Maybe(Terrain),Maybe(Food))
                              >>= (joinFst)                 -- : (Maybe(Terrain),Maybe(Food)) -> Maybe(Terrain, Maybe(Food))


type UnloadF = Ld.UnloadF Tile FoodT

unload : UnloadF    -- : Terrain -> Coords -> Maybe(Terrain, Food)
unload terrain unldrPos = let unload' occ = case occ of
                                                Ant ant -> (Fd.unload ant) >>= (return . (mapFst asAnt)) 
                                                AntNest nest -> (Fd.unload nest) >>= (return . (mapFst asAnt))
                                                _ -> Nothing

                              updateTerrain tile' = terrain `add` tile'
                           in
                              (terrain `get` unldrPos)      -- : Maybe(Tile)
                               >>= (.occupant)              -- : Tile -> Maybe(Occupant)
                               >>= (unload')                -- : Occupant -> Maybe(Tile, Food)
                               >>= (mapFst updateTerrain)   -- : (Tile, Food) -> Maybe(Maybe(Terrain), Food)
                               >>= (joinFst)                -- : (Maybe(Terrain), Food) -> Maybe(Terrain, Food)

type Loader = Ld.Loader Tile

loader : Terrain -> Loader
loader terrain = Ld.loader load unload terrain