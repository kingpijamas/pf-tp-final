module AntColony.Model.Loading where

import open AntColony.Utils.Tuple
import open AntColony.Utils.MaybeMonad
import open AntColony.Model.Data.Terrain
import AntColony.Capacities.Loading as Ld
import open AntColony.Model.Data.Food

import open AntColony.Model.Data.Ant

type LoadF = Ld.LoadF Tile Food
                             
load : LoadF    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
load terrain ldrPos fd = let load' tile = case tile.occupant of
                                              Just(AntTile ant) -> joinFst (ant.cargo `loadFood` fd, asAnt' tile ant)
                                              Just(AntNestTile nest) -> joinFst (nest `loadFood` fd, cast tile asNest)
                                              Just(FoodTile chunk) -> joinFst (chunk `loadFood` fd, cast tile asFood)
                                              Nothing -> return ((foodChunk fd, Nothing), cast tile asFood)
                                              _ -> Nothing

                             asAnt' tile ant (cargo',rem) = return (tile `setOccupant` Just (asAnt (ant `setCargo` cargo')), rem)

                             cast tile castF (occ',rem) = return (tile `setOccupant` ((return . castF) occ'), rem)

                             updateTile ((occ',rem), cast') = cast' (occ',rem) 

                             updateTerrain (tile',rem) = joinFst (terrain `add` tile', rem)
                          in 
                             (terrain `get` ldrPos)    -- : Maybe(Tile)
                              >>= (load')              -- : Tile -> Maybe((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Tile,Maybe(Food)))
                              >>= (updateTile)         -- : ((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Tile,Maybe(Food))) -> Maybe(Tile,Maybe(Food))
                              >>= (updateTerrain)      -- : (Tile, Maybe(Food)) -> Maybe(Terrain,Maybe(Food))



type UnloadF = Ld.UnloadF Tile Food

unload : UnloadF    -- : Terrain -> Coords -> Maybe(Terrain, Food)
unload terrain unldrPos = let unload' tile = case tile.occupant of
                                                    Just(AntTile ant) -> joinFst (unloadFood ant.cargo, asAnt' tile ant)
                                                    Just(AntNestTile nest) -> joinFst (unloadFood nest, cast tile asNest')
                                                    Just(FoodTile chunk) -> joinFst (unloadFood chunk, cast tile (\_ -> Nothing))
                                                    _ -> Nothing

                              asAnt' tile ant (cargo',food) = return (tile `setOccupant` Just (asAnt (ant `setCargo` cargo')), food)
                              
                              asNest' = return . asNest

                              cast tile castF (occ',fd) = return (tile `setOccupant` (castF occ'), fd)

                              updateTile ((occ',fd), cast') = cast' (occ',fd)

                              updateTerrain (tile',fd) = joinFst (terrain `add` tile', fd)
                           in
                              (terrain `get` unldrPos)      -- : Maybe(Tile)
                               >>= (unload')                -- : Tile -> Maybe ((Occupant, Food), (Occupant, Food) -> Maybe(Tile, Food))
                               >>= (updateTile)             -- : ((Occupant, Food), (Occupant, Food) -> Maybe(Tile, Food)) -> Maybe(Tile, Food)
                               >>= (updateTerrain)          -- : (Tile, Food) -> Maybe(Terrain, Food)


type Loader = Ld.Loader Tile -- : SF (LoadSignal) (Maybe(Terrain))

loader : Terrain -> Loader
loader terrain = Ld.loader load unload terrain