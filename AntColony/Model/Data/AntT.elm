module AntColony.Model.Data.AntT where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Model.Data.Food
import open AntColony.Model.Data.Scentable

import open Dict

type Cargo = FoodCarrier {}
type Memory = (Pheromone, Direction)

type AntT = { position : Coords
            , orientation : Direction
            , cargo : Cargo
            , nestPos : Coords
            , remembers : [Memory]
            }

ant : AntT
ant = { position = coords 1 1
      , orientation = N
      , cargo = { food = Nothing
                , limit = Just 1
                }
      , nestPos = coords 0 0
      , remembers = []
      }

setCargo : AntT -> Cargo -> AntT
setCargo ant cargo' = { ant | cargo <- cargo' }

setMemory : AntT -> [Memory] -> AntT
setMemory ant ms = { ant | remembers <- ms }

remember : AntT -> Memory -> AntT
remember ant mem = ant `setMemory` (mem::ant.remembers)

forgetAll : AntT -> AntT
forgetAll ant = ant `setMemory` []
-- SF Terrain Terrain