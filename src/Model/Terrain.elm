module Model.Terrain where

import open Geography.Area

{-- Exposing from area --}
type Terrain = Area Tile
get = get
add = add
remove = remove

type FoodT = Int                      --TODO: move this

type AntNestT = { food:FoodT }        --TODO: move this

data Occupant = Rock
              | Food FoodT
              | Ant AntT
              | AntNest AntNestT

type Pheromone = Int

type Tile = { occupant : Maybe(Occupant)
            , scent : Pheromone
            }