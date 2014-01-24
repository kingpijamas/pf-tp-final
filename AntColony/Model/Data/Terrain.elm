module AntColony.Model.Data.Terrain where

import Dict
import open AntColony.Geography.Area
import open AntColony.Model.Data.Food
import open AntColony.Model.Data.Ant
import open AntColony.Model.Data.AntNest
import open AntColony.Model.Data.Scentable

type Terrain = Area Tile

type Tile = Scentable { occupant : Maybe(Occupant) }

data Occupant = RockTile
              | FoodTile FoodChunk
              | AntTile Ant
              | AntNestTile AntNest

{-- Exposing methods from area --}
add = add
remove = remove
get = get



terrain : Int -> Int -> [(Coords,Tile)]-> Terrain
terrain width height tiles = area width height tiles

empty : Int -> Int -> Terrain
empty width height = empty width height

tile : Maybe(Occupant) -> Maybe(Pheromone) -> Tile
tile occ scent = { occupant=occ
                 , scent=scent
                 }

fmap : (Maybe(Occupant) -> Maybe(Occupant)) -> Tile -> Tile
fmap f tile = { tile | occupant <- (f (tile.occupant)) }

setOccupant : Tile -> Maybe(Occupant) -> Tile
setOccupant tile occ = (\_ -> occ) `fmap` tile

asAnt : Ant -> Occupant
asAnt x = AntTile x

asNest : AntNest -> Occupant
asNest x = AntNestTile x

asFood : FoodChunk -> Occupant
asFood x = FoodTile x