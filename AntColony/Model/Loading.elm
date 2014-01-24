module AntColony.Model.Loading where

import open AntColony.Utils.SignalFunction
import open AntColony.Utils.MaybeMonad
import open AntColony.Utils.Tuple

import open AntColony.Capacities.Positioning

import open AntColony.Geography.Area

import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.Food
import open AntColony.Model.Data.Ant

data LoadAction = Load | Unload

type LoadSignal  = { from:Coords
                   , target:Coords
                   , action:LoadAction
                   }

asLoadSignal : LoadAction -> Coords -> Coords -> LoadSignal
asLoadSignal action from target = {from=from, target=target, action=action}

loadSignal : Coords -> Coords -> LoadSignal
loadSignal = asLoadSignal Load

unloadSignal : Coords -> Coords -> LoadSignal
unloadSignal = asLoadSignal Unload

asLocationSignal : LoadSignal -> LocationSignal
asLocationSignal sig = locationSignal (sig.from) (sig.target)



loadFacade : Terrain -> LoadSignal -> Maybe(Terrain)
loadFacade terrain sig = let sig' = asLocationSignal sig
                       in
                          case sig.action of
                               Load   -> load terrain sig'
                               Unload -> unload terrain sig'



load : Terrain -> LocationSignal -> Maybe(Terrain)
load terrain sig = let ldrPos = sig.from
                       unldrPos = sig.target

                       load' ldrPos (terrain',cargo) = ld terrain' ldrPos cargo

                       returnRemnant unldrPos (terrain'',rem) = case rem of
                                                                     Just rem' -> load' unldrPos (terrain'',rem')
                                                                     _ -> return (terrain'',Nothing)
                    in
                       (terrain `unld` unldrPos)             -- : Maybe(Terrain,Food)
                        >>= (load' ldrPos)                   -- : (Terrain,Food) -> Maybe(Terrain, Maybe(Food))
                        >>= (returnRemnant unldrPos)         -- : (Terrain,Food) -> Maybe(Terrain, Maybe(Food))
                        >>= (return . fst)                   -- : (Terrain, Maybe(Food)) -> Maybe (Terrain)


unload : Terrain -> LocationSignal -> Maybe(Terrain)
unload terrain sig = load terrain (locationSignal sig.target sig.from)




ld : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
ld terrain ldrPos fd = let load' tile = case tile.occupant of
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
                           (terrain `get` ldrPos)            -- : Maybe(Tile)
                            >>= (load')                      -- : Tile -> Maybe((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Tile,Maybe(Food)))
                            >>= (updateTile)                 -- : ((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Tile,Maybe(Food))) -> Maybe(Tile,Maybe(Food))
                            >>= (updateTerrain)              -- : (Tile, Maybe(Food)) -> Maybe(Terrain,Maybe(Food))


unld : Terrain -> Coords -> Maybe(Terrain, Food)    -- : Terrain -> Coords -> Maybe(Terrain, Food)
unld terrain unldrPos = let unload' tile = case tile.occupant of
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
                           (terrain `get` unldrPos)          -- : Maybe(Tile)
                            >>= (unload')                    -- : Tile -> Maybe ((Occupant, Food), (Occupant, Food) -> Maybe(Tile, Food))
                            >>= (updateTile)                 -- : ((Occupant, Food), (Occupant, Food) -> Maybe(Tile, Food)) -> Maybe(Tile, Food)
                            >>= (updateTerrain)              -- : (Tile, Food) -> Maybe(Terrain, Food)



--type Loader a = SF (LoadSignal) (Maybe(Terrain))

--loader : (LoadF a Food) -> (UnloadF a Food) -> (Terrain) -> (Loader a)
--loader ld unld terrain = arr (loadProxy ld unld terrain)


--type Loader = Ld.Loader Tile -- : SF (LoadSignal) (Maybe(Terrain))

--loader : Terrain -> Loader
--loader terrain = Ld.loader load unload terrain