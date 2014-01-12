module Model.Terrain where

import open Geography.Area
import open Model.Food
import open Model.Ant.Ant
import open Model.AntNest

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

asAnt : AntT -> Occupant
asAnt x = Ant x

asNest : AntNestT -> Occupant
asNest x = AntNest x