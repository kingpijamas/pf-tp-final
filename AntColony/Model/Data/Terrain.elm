module AntColony.Model.Data.Terrain where

import Dict
import open AntColony.Geography.Area
import open AntColony.Model.Data.Food
import open AntColony.Model.Data.AntT
import open AntColony.Model.Data.AntNestT
import open AntColony.Model.Data.Scentable

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

type Position = Scentable { occupant : Maybe(Occupant) }

data Occupant = Rock
              | FoodChunk FoodChunkT
              | Ant AntT
              | AntNest AntNestT

positionFor : Occupant -> Position
positionFor occ = position (Just occ) Nothing

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

asAnt : AntT -> Occupant
asAnt x = Ant x

asNest : AntNestT -> Occupant
asNest x = AntNest x

asFood : FoodChunkT -> Occupant
asFood x = FoodChunk x

getAnts : Terrain -> [Occupant]
getAnts terrain = Dict.values terrain.elems |> asOccupants
    |> filter (\occupant -> case occupant of 
                                Just (Ant ant) -> True
                                _ -> False)
    |> map (\maybeOcc -> case maybeOcc of Just x -> x)

asOccupants : [Position] -> [Maybe Occupant]
asOccupants = map (\scentable -> scentable.occupant) 



