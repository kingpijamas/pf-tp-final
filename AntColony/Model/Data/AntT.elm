module AntColony.Model.Data.AntT where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Model.Data.Food
import open AntColony.Model.Data.Scentable

import open Dict

type Cargo = FoodCarrier {}

type AntT = { position : Coords
            , orientation : Direction
            , cargo : Cargo
            , nestPos : Coords
            }

ant : AntT
ant = { position = coords 1 1
      , orientation = N
      , cargo = { food = Nothing
                , limit = Just 1
                }
      , nestPos = coords 0 0
      }

setCargo : AntT -> Cargo -> AntT
setCargo ant cargo' = { ant | cargo <- cargo' }