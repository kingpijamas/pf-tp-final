module AntColony.Model.Terrain where

import Dict
import open AntColony.Geography.Area
import open AntColony.Model.Food
import open AntColony.Model.Ant.Ant
import open AntColony.Model.AntNest
import open AntColony.Model.FoodChunk

{-- Exposing from area --}
type Terrain = {tiles : Area Tile, tileSize : Int}
type Tile = {occupant : Maybe Occupant, scent : Maybe Pheromone}

type Pheromone = Int
data Occupant = RockTile
              | FoodTile FoodChunk
              | AntTile Ant
              | AntNestTile AntNest

newTerrain : Terrain
newTerrain = let 
                ant1 = ant
                elems = Dict.empty 
                    |> Dict.insert (coords 1 1) (tile (Just RockTile) Nothing)
                    |> Dict.insert (coords 1 2) (tile (Just (AntTile ant)) Nothing)
                    |> Dict.insert (coords 2 2) (tile (Just (AntTile ant)) Nothing)
             in {tiles = area elems 4 4, tileSize = 20}

asTileList : Terrain -> [(Coords, Tile)]
asTileList terrain = Dict.toList terrain.tiles.elems

getAnts : Terrain -> [Coords]
getAnts terrain = [coords 1 1, coords 2 2]     -- FIXME: dummy logic

getTiles : Terrain -> [Coords]
getTiles terrain = [coords 1 1, coords 1 2, coords 2 1, coords 2 2, coords 3 1, coords 3 2]    -- FIXME: dummy logic

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

