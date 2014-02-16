module AntColony.Logic.Loading where

import open AntColony.Utils.SignalFunction
import open AntColony.Utils.Maybe
import open AntColony.Utils.Tuple



import open AntColony.Geography.Coords
import open AntColony.Geography.Coords
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
ld : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
ld terrain ldrPos fd = (terrain `get` ldrPos)                                 -- Maybe(Position) 
--                        >>= (updateOccupant (\occupant -> Just occupant))    -- Position -> Maybe(Position)
                        >>= (\x -> Just (terrain, Nothing))

{-
    let load' pos = case pos.occupant of
                                            Just(Ant ant) -> joinFst (loadFood ant.cargo fd, asAnt' pos ant)
                                            Just(AntNest nest) -> joinFst (loadFood nest fd, cast pos asNest)
                                            Just(FoodChunk chunk) -> joinFst (loadFood chunk fd, cast pos asFood)
                                            Nothing -> return ((foodChunk fd, Nothing), cast pos asFood)
                                            _ -> Nothing
                           asAnt' pos ant (cargo',rem) = Nothing -- return (pos `setOccupant2` (asAnt (ant `setCargo` cargo')), rem)
                           cast pos castF (occ',rem) = Nothing -- return (pos `setOccupant2` (castF occ'), rem)
                           updatePos ((occ',rem), cast') = cast' (occ',rem) 
                           updateTerrain (pos',rem) = joinFst (terrain `add` pos', rem)
                        in
                           (terrain `get` ldrPos)            -- : Maybe(Position)
                            >>= load'                        -- : Position -> Maybe((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Position,Maybe(Food)))
                            >>= updatePos                    -- : ((Occupant, Maybe(Food)), (Occupant, Maybe(Food))->Maybe(Position,Maybe(Food))) -> Maybe(Position,Maybe(Food))
                            >>= updateTerrain                -- : (Position, Maybe(Food)) -> Maybe(Terrain,Maybe(Food))
-}

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
                            >>= unload'                      -- : Position -> Maybe ((Occupant, Food), (Occupant, Food) -> Maybe(Position, Food))
                            >>= updatePos                    -- : ((Occupant, Food), (Occupant, Food) -> Maybe(Position, Food)) -> Maybe(Position, Food)
                            >>= updateTerrain                -- : (Position, Food) -> Maybe(Terrain, Food)

