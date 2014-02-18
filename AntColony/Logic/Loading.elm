module AntColony.Logic.Loading where

import open AntColony.Utils.SignalFunction
import open AntColony.Utils.Maybe
import open AntColony.Utils.Tuple

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import AntColony.Model.FoodCarrier as Carrier

import open AntColony.Model.Terrain
import open AntColony.Model.Food
import open AntColony.Model.AntT
import open AntColony.Model.FoodChunkT

loadInDir : Terrain -> Coords -> Direction -> Maybe(Terrain)
loadInDir terrain from dir = (from `addDir` dir)      -- : Maybe(Coords)
                              >>= (load terrain from) -- : Coords -> Maybe(Terrain)

unloadInDir : Terrain -> Coords -> Direction -> Maybe(Terrain)
unloadInDir terrain from dir = (from `addDir` dir)        -- : Maybe(Coords)
                                >>= (unload terrain from) -- : Coords -> Maybe(Terrain)

load : Terrain -> Coords -> Coords -> Maybe(Terrain)
load terrain ldrPos unldrPos = let load' ldrPos (terrain',cargo) = ld terrain' ldrPos cargo

                                   returnRemnantTo unldrPos (terrain'',rem) = case rem of
                                                                                   Just rem' -> load' unldrPos (terrain'',rem')
                                                                                   _ -> return (terrain'', Nothing)

                                   getTerrain (terrain',_) = return terrain'
                                in
                                   (terrain `unld` unldrPos)         -- : Maybe(Terrain,Food)
                                    >>= (load' ldrPos)               -- : (Terrain,Food) -> Maybe(Terrain, Maybe(Food))
                                    >>= (returnRemnantTo unldrPos)   -- : (Terrain,Food) -> Maybe(Terrain, Maybe(Food))
                                    >>= getTerrain                   -- : (Terrain, Maybe(Food)) -> Maybe (Terrain)

unload : Terrain -> Coords -> Coords -> Maybe(Terrain)
unload terrain unldrPos ldrPos = load terrain ldrPos unldrPos



{-- Auxiliary, model-specific methods --}
unld : Terrain -> Coords -> Maybe(Terrain, Food)
unld terrain unldrCoords = let unloadPos pos = case pos.occupant of 
                                                 Just(Ant ant) -> doUnload (ant.cargo) (updateAnt pos ant)
                                                 Just(FoodChunk chunk) -> doUnload chunk (updateFoodChunk pos)
                                                 _ -> Nothing

                               doUnload unldee updateF = Carrier.unload unldee >>=^ (mapFst updateF)

                               updateAnt pos ant cargo' = pos `setOccupant2` (Ant (ant `setCargo` cargo'))
                               updateFoodChunk pos chunk' = empty pos

                               updateTerrain (pos', food) = joinFst (add terrain unldrCoords pos', food)
                            in
                               (terrain `get` unldrCoords)       -- : Maybe(Position)
                                >>= unloadPos                    -- : Position -> Maybe(Position, Food)
                                >>= updateTerrain                -- : (Position, Food) -> Maybe(Terrain, Food)

ld : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
ld terrain ldrCoords food = let loadPos pos = case pos.occupant of
                                                   Just(Ant ant) -> doLoad (ant.cargo) (updateAnt pos ant)
                                                   Just(AntNest nest) -> doLoad nest (updateNest pos)
                                                   --Just(FoodChunk chunk) -> doLoad chunk (updateFoodChunk pos)
                                                   Nothing -> return (updateFoodChunk pos (foodChunk food), Nothing)
                                                   _ -> Nothing

                                doLoad ldee updateF = Carrier.load ldee food >>=^ (mapFst updateF)

                                updateAnt pos ant cargo' = pos `setOccupant2` (Ant (ant `setCargo` cargo'))
                                updateNest pos nest' = pos `setOccupant2` (AntNest nest')
                                updateFoodChunk pos chunk' = pos `setOccupant2` (FoodChunk chunk')

                                updateTerrain (pos', mbfood) = joinFst (add terrain ldrCoords pos', mbfood)
                             in
                                (terrain `get` ldrCoords)         -- : Maybe(Position)
                                 >>= loadPos                      -- : Position -> Maybe(Position, Maybe(Food))
                                 >>= updateTerrain                -- : (Position, Maybe(Food)) -> Maybe(Terrain, Maybe(Food))
