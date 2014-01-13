module Model.Ant.Loading where

import open Utils.Tuple
import open Utils.MaybeMonad
import open Model.Terrain
import Capacities.Loading as Ld
import open Model.Food

type LoadF = Ld.LoadF Tile Food
                             
load : LoadF    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
load terrain ldrPos fd = let loadOcc tile = case tile.occupant of
                                                Just(AntT ant) -> joinFst (ant `loadFood` fd, cast tile asAnt)             -- : Maybe(AntT, Maybe(Food))
                                                Just(AntNestT nest) -> joinFst (nest `loadFood` fd, cast tile asNest)      -- : Maybe(AntNestT, c)
                                                _ -> Nothing

                             cast tile castF (occ',rem) = return (tile `setOccupant` ((return . castF) occ'), rem)

                             updateTile ((occ',rem), cast') = cast' (occ',rem) 

                             updateTerrain (tile',rem) = joinFst (terrain `add` tile', rem)
                          in 
                             (terrain `get` ldrPos)                    -- : Maybe(Tile)
                              >>= (loadOcc)                            -- : Tile -> Maybe((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Tile,Maybe(Food)))
                              >>= (updateTile)                         -- : ((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Tile,Maybe(Food))) -> Maybe(Tile,Maybe(Food))
                              >>= (updateTerrain)                      -- : (Tile, Maybe(Food)) -> Maybe(Terrain,Maybe(Food))



type UnloadF = Ld.UnloadF Tile Food

unload : UnloadF    -- : Terrain -> Coords -> Maybe(Terrain, Food)
unload terrain unldrPos = let unloadOcc tile = case tile.occupant of
                                                    Just(AntT ant) -> joinFst (unloadFood ant, cast tile asAnt) 
                                                    Just(AntNestT nest) -> joinFst (unloadFood nest, cast tile asNest)
                                                    _ -> Nothing

                              cast tile castF (occ',fd) = return (tile `setOccupant` ((return . castF) occ'), fd)

                              updateTile ((occ',fd), cast') = cast' (occ',fd)

                              updateTerrain (tile',fd) = joinFst (terrain `add` tile', fd)
                           in
                              (terrain `get` unldrPos)      -- : Maybe(Tile)
                               >>= (unloadOcc)              -- : Tile -> Maybe ((Occupant, Food), (Occupant, Food) -> Maybe(Tile, Food))
                               >>= (updateTile)             -- : (Occupant, Food) -> Maybe(Tile, Food)
                               >>= (updateTerrain)          -- : (Tile, Food) -> Maybe(Terrain, Food)


type Loader = Ld.Loader Tile

loader : Terrain -> Loader
loader terrain = Ld.loader load unload terrain