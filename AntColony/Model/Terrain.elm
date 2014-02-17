module AntColony.Model.Terrain where

import AntColony.Geography.Area as Area
import open AntColony.Geography.Coords
import open AntColony.Model.Food
import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.Scent

import open AntColony.Utils.Maybe

type Terrain = { area : Area.Area Position 
               , debug : String --TODO: for debugging!
               }

--TODO : for debugging!
log : Terrain -> String -> Terrain
log terrain text = { terrain | debug <- (terrain.debug ++ text) }

{-- Exposing methods from terrain --}
isWithinBounds : Coords -> Terrain -> Bool
isWithinBounds coords terrain = Area.isWithinBounds coords terrain.area

add : Terrain -> Coords -> Position -> Maybe(Terrain)
add terrain coords pos = (Area.add terrain.area coords pos)       -- : Maybe(Area Position)
                          >>= (\area' -> return { area = area'
                                                , debug = terrain.debug
                                                }) -- : Area -> Maybe(Terrain)

get : Terrain -> Coords -> Maybe(Position)
get terrain coords = let mbpos = Area.get terrain.area coords
                      in
                         case mbpos of
                              Just pos -> Just pos
                              Nothing -> if (isWithinBounds coords terrain) 
                                         then Just (position Nothing Nothing)
                                         else Nothing

remove : Terrain -> Coords -> Maybe(Terrain)
remove terrain coords = (Area.remove terrain.area coords)         -- : Maybe(Area Position)
                          >>= (\area' -> return { area = area'
                                                , debug = terrain.debug
                                                }) -- : Area -> Maybe(Terrain)

values : Terrain -> [Position]
values terrain = Area.values terrain.area

toList : Terrain -> [(Coords,Position)]
toList terrain = Area.toList terrain.area

terrain : Int -> Int -> [(Coords,Position)]-> Terrain
terrain width height tiles = { area = Area.area width height tiles 
                             , debug = ""
                             }

--TODO: for testing!
terrainMF : Int -> Int -> [(Coords,Position)] -> Terrain
terrainMF = terrain

width : Terrain -> Int
width terrain = terrain.area.width

height : Terrain -> Int
height terrain = terrain.area.height

{--empty : Int -> Int -> Terrain
empty width height = empty width height
--}

{-- Position --}
type Position = Scentable { occupant : Maybe(Occupant) }

data Occupant = Rock
              | FoodChunk FoodChunkT
              | Ant AntT
              | AntNest AntNestT

position : Maybe(Occupant) -> Maybe(Pheromone) -> Position
position occ scent = { occupant = occ
                     , scent = scent
                     }

updateOccupant : Position -> (Maybe(Occupant)->Maybe(Occupant)) -> Position
updateOccupant pos update = pos `setOccupant` (update pos.occupant)

getOccupant : Position -> Maybe(Occupant)
getOccupant pos = pos.occupant

setOccupant : Position -> Maybe(Occupant) -> Position
setOccupant pos occ = { pos | occupant <- occ }

setOccupant2 : Position -> Occupant -> Position
setOccupant2 pos occ = { pos | occupant <- Just occ }

empty : Position -> Position
empty pos = { pos | occupant <- Nothing }

{-- Casts --}
asAnt : AntT -> Occupant
asAnt x = Ant x

asNest : AntNestT -> Occupant
asNest x = AntNest x

asFood : FoodChunkT -> Occupant
asFood x = FoodChunk x

getAnts : Terrain -> [Occupant]
getAnts terrain = let hasAnt pos = case pos.occupant of
                                        Just (Ant _) -> True
                                        _ -> False

                      justAntPoss = filter hasAnt (values terrain)

                      decont justAntPos = case justAntPos.occupant of
                                               Just ant -> ant
                   in
                      map decont justAntPoss

getFood : Terrain -> [Occupant]
getFood terrain = let hasFood pos = case pos.occupant of
                                         Just (FoodChunk _) -> True
                                         _ -> False

                      justFoodPoss = filter hasFood (values terrain)


                      decont justFoodPos = case justFoodPos.occupant of
                                                Just foodChunk -> foodChunk
                   in
                      map decont justFoodPoss


getScent : Terrain -> Coords -> Maybe Int
getScent terrain from = case (terrain `get` from) of
                             Just p -> p.scent
                             _ -> Nothing

