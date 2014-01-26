module AntColony.Model.Data.AntT where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Model.Data.Food

type Cargo = FoodCarrier {}

type AntT = { position : Coords
            , orientation : Direction
            , nestPos : Coords
            , cargo : Cargo
            }

ant : AntT
ant = { position = coords 1 1
      , orientation = N
      , nestPos = coords 0 0
      , cargo = { food = Nothing
                , limit = Just 1
                }
      }

setCargo : AntT -> Cargo -> AntT
setCargo ant cargo' = { ant | cargo <- cargo' }

-- SF Terrain Terrain