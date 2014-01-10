module Model.Terrain where

import open Geography.Area

{-- Exposing from area --}
type Terrain = Area Tile
get = get
add = add
remove = remove

data Occupant = Rock | Food | Ant | AntNest

type Pheromone = Int

type Tile = { occupant : Maybe(Occupant)
            , scent : Pheromone
            }