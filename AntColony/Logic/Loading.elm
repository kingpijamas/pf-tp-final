module AntColony.Logic.Loading where

import open AntColony.Utils.SignalFunction
import open AntColony.Utils.Maybe
import open AntColony.Utils.Tuple

import open AntColony.Geography.Coords


import open AntColony.Model.Terrain
import open AntColony.Model.Food
import open AntColony.Model.AntT

load : Terrain -> Coords -> Coords -> Maybe(Terrain)
load terrain ldrPos unldrPos = let load' ldrPos (terrain',cargo) = ld terrain' ldrPos cargo

                                   returnRemnantTo unldrPos (terrain'',rem) = case rem of
                                                                                   Just rem' -> load' unldrPos (terrain'',rem')
                                                                                   _ -> return (terrain'',Nothing)

                                   getTerrain (terrain',_) = return terrain'
                                in
                                   (terrain `unld` unldrPos)         -- : Maybe(Terrain,Food)
                                    >>= (load' ldrPos)               -- : (Terrain,Food) -> Maybe(Terrain, Maybe(Food))
                                    >>= (returnRemnantTo unldrPos)   -- : (Terrain,Food) -> Maybe(Terrain, Maybe(Food))
                                    >>= getTerrain                   -- : (Terrain, Maybe(Food)) -> Maybe (Terrain)


unload : Terrain -> Coords -> Coords -> Maybe(Terrain)
unload terrain unldrPos ldrPos = load terrain ldrPos unldrPos

{-- Auxiliary, model-specific methods --}
ld : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
ld terrain ldrCoords food = let --loadFood : (FoodCarrier a) -> Food -> Maybe (FoodCarrier a, Maybe(Food))
                                loadPos : Position -> Maybe(Position, Maybe(Food))
                                loadPos pos = case pos.occupant of
                                                   Just(Ant ant) -> doLoad (ant.cargo) (updateAnt pos ant)
                                                   Just(AntNest nest) -> doLoad nest (updateNest pos)
                                                   Just(FoodChunk chunk) -> doLoad chunk (updateFoodChunk pos)
                                                   _ -> Nothing 


                                doLoad ldee updateF = loadFood ldee food >>=^ (mapFst updateF)

                                updateAnt pos ant cargo' = pos `setOccupant2` (Ant (ant `setCargo` cargo'))
                                updateNest pos nest' = pos `setOccupant2` (AntNest nest')
                                updateFoodChunk pos chunk' = pos `setOccupant2` (FoodChunk chunk')

                                updateTerrain (pos', mbfood) = joinFst (add terrain ldrCoords pos', mbfood)

                           in
                              (terrain `get` ldrCoords)         -- : Maybe(Position)
                               >>= loadPos                      -- : Position -> Maybe(Position, Maybe(Food))
                               >>= updateTerrain                -- : (Position, Maybe(Food)) -> Maybe(Terrain, Maybe(Food))

unld : Terrain -> Coords -> Maybe(Terrain, Food)
unld terrain unldrPos = let unload' pos = case pos.occupant of
                                               Just(Ant ant) -> joinFst (unloadFood ant.cargo, asAnt' pos ant)
                                               Just(AntNest nest) -> joinFst (unloadFood nest, cast pos asNest')
                                               Just(FoodChunk chunk) -> joinFst (unloadFood chunk, cast pos (\_ -> Nothing))
                                               _ -> Nothing

                            asAnt' pos ant (cargo',food) = return (pos `setOccupant` Just (asAnt (ant `setCargo` cargo')), food)
                            asNest' = return . asNest
                            cast pos castF (occ',food) = return (pos `setOccupant` (castF occ'), food)

                            updatePos ((occ',food), cast') = cast' (occ',food)

                            updateTerrain (pos',food) = joinFst (add terrain unldrPos pos', food)
                        in
                           (terrain `get` unldrPos)          -- : Maybe(Position)
                            >>= unload'                      -- : Position -> Maybe ((Occupant, Food), (Occupant, Food) -> Maybe(Position, Food))
                            >>= updatePos                    -- : ((Occupant, Food), (Occupant, Food) -> Maybe(Position, Food)) -> Maybe(Position, Food)
                            >>= updateTerrain                -- : (Position, Food) -> Maybe(Terrain, Food)

