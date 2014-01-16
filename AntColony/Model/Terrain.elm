module AntColony.Model.Terrain where

import Dict
-- import open AntColony.Geography.Area
import open AntColony.Utils.Matrix
import open AntColony.Model.Food
import open AntColony.Model.Ant.Ant
import open AntColony.Model.AntNest
import open AntColony.Model.FoodChunk

{-- Exposing from area --}
type Terrain = {tiles : Matrix Tile, tileSize : Int}
type Tile = {occupant : Maybe Occupant, scent : Maybe Pheromone}

type Pheromone = Int
data Occupant = RockTile
              | FoodTile FoodChunk
              | AntTile Ant
              | AntNestTile AntNest

newTerrain : Terrain
newTerrain = {tiles = matrix Dict.empty 4 4, tileSize = 20}

asTileList : Terrain -> [(Position, Tile)]
asTileList terrain = Dict.toList terrain.tiles.elems

getAnts : Terrain -> [Position]
getAnts terrain = [position 1 1, position 2 2]     -- FIXME: dummy logic

getTiles : Terrain -> [Position]
getTiles terrain = [position 1 1, position 1 2, position 2 1, position 2 2, position 3 1, position 3 2]    -- FIXME: dummy logic

tileSize : Terrain -> Int
tileSize terrain = terrain.tileSize

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

