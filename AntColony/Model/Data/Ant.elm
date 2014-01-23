module AntColony.Model.Data.Ant where

import open AntColony.Model.Data.Food
import open AntColony.Geography.Area
import open AntColony.Geography.Direction

type Cargo = FoodCarrier {}

type Ant = { position : Coords
           , orientation : Direction
           , cargo : Cargo
           }

ant : Ant
ant = { position = coords 0 0
      , orientation = N
      , cargo = { food = Nothing
                , limit = Just 1
                }
      }

setCargo : Ant -> Cargo -> Ant
setCargo ant cargo' = { ant | cargo <- cargo' }

-- SF Terrain Terrain