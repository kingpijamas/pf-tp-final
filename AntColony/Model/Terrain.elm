module AntColony.Model.Terrain where

import open AntColony.Geography.Area
import open AntColony.Model.Food
import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.Scentable

type Terrain = Area Position

{-- Exposing methods from terrain --}
add = add
remove = remove
get = get

terrain : Int -> Int -> [(Coords,Position)]-> Terrain
terrain width height tiles = area width height tiles

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

setOccupant : Position -> Maybe(Occupant) -> Position
setOccupant pos occ = { pos | occupant <- occ }

setOccupant' : Position -> Occupant -> Position
setOccupant' pos occ = { pos | occupant <- Just occ }

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
getAnts terrain = values terrain |> map (\position -> position.occupant)
                                 |> filter (\occupant -> case occupant of 
                                                              Just (Ant ant) -> True
                                                              _ -> False
                                           )
                                 |> map (\mbOcc -> case mbOcc of
                                                        Just x -> x
                                        ) -- FIXME: what about the Nothing case?