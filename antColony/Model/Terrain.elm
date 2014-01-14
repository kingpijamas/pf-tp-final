module Model.Terrain where

import open Geography.Area
import open Model.Food
import open Model.Ant.Ant
import open Model.AntNest
import open Model.FoodChunk

{-- Exposing from area --}
type Terrain = Area Tile
get = get
add = add
remove = remove

data Occupant = RockTile
              | FoodTile FoodChunk
              | AntTile Ant
              | AntNestTile AntNest

type Pheromone = Int

type Tile = { occupant : Maybe(Occupant)
            , scent : Maybe(Pheromone)
            }

tile : Maybe(Occupant) -> Maybe(Pheromone) -> Tile
tile occ scent = { occupant=occ, scent=scent }

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