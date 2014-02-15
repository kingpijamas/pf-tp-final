module AntColony.Model.Terrain where

import open AntColony.Geography.Area
import open AntColony.Model.Food
import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.Scent

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

updateOccupant : Position -> (Maybe(Occupant)->Maybe(Occupant)) -> Position
updateOccupant pos update = setOccupant pos (update pos.occupant)

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
asAnt x = Rock

asNest : AntNestT -> Occupant
asNest x = Rock

asFood : FoodChunkT -> Occupant
asFood x = Rock

getAnts : Terrain -> [Occupant]
getAnts terrain = let justAntPoss = filter hasAnt (values terrain)
                      
                      hasAnt pos = case pos.occupant of
                                        Just (Ant _) -> True
                                        _ -> False

                      decont justAntPos = case justAntPos.occupant of
                                               Just ant -> ant
                   in
                      map decont justAntPoss


