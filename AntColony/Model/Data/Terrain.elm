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
terrain width height tiles = terrain width height tiles

empty : Int -> Int -> Terrain
empty width height = empty width height


type Position = Scentable { occupant : Maybe(Occupant) }

data Occupant = Rock
              | FoodChunk FoodChunkT
              | Ant AntT
              | AntNest AntNestT

position : Maybe(Occupant) -> Maybe(Pheromone) -> Position
position occ scent = { occupant = occ
                     , scent = scent
                     }

fmap : (Maybe(Occupant) -> Maybe(Occupant)) -> Position -> Position
fmap f pos = { pos | occupant <- (f (pos.occupant)) }

setOccupant : Position -> Maybe(Occupant) -> Position
setOccupant pos occ = (\_ -> occ) `fmap` pos

asAnt : AntT -> Occupant
asAnt x = Ant x

asNest : AntNestT -> Occupant
asNest x = AntNest x

asFood : FoodChunkT -> Occupant
asFood x = FoodChunk x