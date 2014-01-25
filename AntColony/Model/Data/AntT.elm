module AntColony.Model.Data.AntT where

import open AntColony.Model.Data.Food
import open AntColony.Geography.Area
import open AntColony.Geography.Direction

type Cargo = FoodCarrier {}

type AntT = { position : Coords
            , orientation : Direction
            , cargo : Cargo
            }

ant : AntT
ant = { position = coords 0 0
      , orientation = N
      , cargo = { food = Nothing
                , limit = Just 1
                }
      }

setCargo : AntT -> Cargo -> AntT
setCargo ant cargo' = { ant | cargo <- cargo' }

-- SF Terrain Terrain