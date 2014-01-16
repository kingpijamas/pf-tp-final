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
newTerrain = let 
                someFood = foodChunk (newFood 5)
                elems = Dict.empty 
                    |> Dict.insert (position 1 1) (tile (Just RockTile) Nothing)
                    |> Dict.insert (position 1 2) (tile (Just (AntTile ant)) Nothing)
                    |> Dict.insert (position 2 2) (tile (Just (AntTile ant)) Nothing)
                    |> Dict.insert (position 4 4) (tile (Just (AntNestTile antNest)) Nothing)
                    |> Dict.insert (position 4 1) (tile (Just (FoodTile someFood)) Nothing)
             in {tiles = matrix elems 4 4, tileSize = 20}

asTileList : Terrain -> [(Position, Tile)]
asTileList terrain = Dict.toList terrain.tiles.elems

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

