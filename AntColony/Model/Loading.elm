module AntColony.Model.Loading where

import open AntColony.Utils.SignalFunction
import open AntColony.Utils.MaybeMonad
import open AntColony.Utils.Tuple

import open AntColony.Capacities.Positioning

import open AntColony.Geography.Area

import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.Food
import open AntColony.Model.Data.AntT

data LoadAction = Load | Unload

type LoadIntent  = { from:Coords
                   , target:Coords
                   , action:LoadAction
                   }

asLoadIntent : LoadAction -> Coords -> Coords -> LoadIntent
asLoadIntent action from target = { from = from
                                  , target = target
                                  , action = action
                                  }

loadIntent : Coords -> Coords -> LoadIntent
loadIntent = asLoadIntent Load

unloadIntent : Coords -> Coords -> LoadIntent
unloadIntent = asLoadIntent Unload

asLocationIntent : LoadIntent -> LocationIntent
asLocationIntent sig = locationIntent (sig.from) (sig.target)



loadFacade : Terrain -> LoadIntent -> Maybe(Terrain)
loadFacade terrain sig = let sig' = asLocationIntent sig
                          in
                             case sig.action of
                                  Load   -> load terrain sig'
                                  Unload -> unload terrain sig'



load : Terrain -> LocationIntent -> Maybe(Terrain)
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


unload : Terrain -> LocationIntent -> Maybe(Terrain)
unload terrain sig = load terrain (locationIntent sig.target sig.from)




ld : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
ld terrain ldrPos fd = let load' pos = case pos.occupant of
                                            Just(Ant ant) -> joinFst (ant.cargo `loadFood` fd, asAnt' pos ant)
                                            Just(AntNest nest) -> joinFst (nest `loadFood` fd, cast pos asNest)
                                            Just(FoodChunk chunk) -> joinFst (chunk `loadFood` fd, cast pos asFood)
                                            Nothing -> return ((foodChunk fd, Nothing), cast pos asFood)
                                            _ -> Nothing

                           asAnt' pos ant (cargo',rem) = return (pos `setOccupant` Just (asAnt (ant `setCargo` cargo')), rem)

                           cast pos castF (occ',rem) = return (pos `setOccupant` ((return . castF) occ'), rem)

                           updatePos ((occ',rem), cast') = cast' (occ',rem) 

                           updateTerrain (pos',rem) = joinFst (terrain `add` pos', rem)
                        in 
                           (terrain `get` ldrPos)            -- : Maybe(Position)
                            >>= (load')                      -- : Position -> Maybe((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Position,Maybe(Food)))
                            >>= (updatePos)                  -- : ((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Position,Maybe(Food))) -> Maybe(Position,Maybe(Food))
                            >>= (updateTerrain)              -- : (Position, Maybe(Food)) -> Maybe(Terrain,Maybe(Food))


unld : Terrain -> Coords -> Maybe(Terrain, Food)    -- : Terrain -> Coords -> Maybe(Terrain, Food)
unld terrain unldrPos = let unload' pos = case pos.occupant of
                                               Just(Ant ant) -> joinFst (unloadFood ant.cargo, asAnt' pos ant)
                                               Just(AntNest nest) -> joinFst (unloadFood nest, cast pos asNest')
                                               Just(FoodChunk chunk) -> joinFst (unloadFood chunk, cast pos (\_ -> Nothing))
                                               _ -> Nothing

                            asAnt' pos ant (cargo',food) = return (pos `setOccupant` Just (asAnt (ant `setCargo` cargo')), food)
                            
                            asNest' = return . asNest

                            cast pos castF (occ',fd) = return (pos `setOccupant` (castF occ'), fd)

                            updatePos ((occ',fd), cast') = cast' (occ',fd)

                            updateTerrain (pos',fd) = joinFst (terrain `add` pos', fd)
                        in
                           (terrain `get` unldrPos)          -- : Maybe(Position)
                            >>= (unload')                    -- : Position -> Maybe ((Occupant, Food), (Occupant, Food) -> Maybe(Position, Food))
                            >>= (updatePos)                  -- : ((Occupant, Food), (Occupant, Food) -> Maybe(Position, Food)) -> Maybe(Position, Food)
                            >>= (updateTerrain)              -- : (Position, Food) -> Maybe(Terrain, Food)



--type Loader a = SF (LoadIntent) (Maybe(Terrain))

--loader : (LoadF a Food) -> (UnloadF a Food) -> (Terrain) -> (Loader a)
--loader ld unld terrain = arr (loadProxy ld unld terrain)


--type Loader = Ld.Loader Position -- : SF (LoadIntent) (Maybe(Terrain))

--loader : Terrain -> Loader
--loader terrain = Ld.loader load unload terrain