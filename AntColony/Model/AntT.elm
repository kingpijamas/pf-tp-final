module AntColony.Model.AntT where

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import open AntColony.Model.FoodCarrier
import open AntColony.Model.Scentable

import open Dict

type Cargo = FoodCarrier {}

type AntT = { position : Coords
            , orientation : Direction
            , cargo : Cargo
            , nestPos : Coords
            }

ant : Coords -> Coords -> Direction -> AntT
ant nestPos position orientation = { position = position
                                   , orientation = orientation
                                   , cargo = { food = Nothing
                                             , limit = Just 1
                                             }
                                   , nestPos = nestPos
                                   }

setCargo : AntT -> Cargo -> AntT
setCargo ant cargo' = { ant | cargo <- cargo' }

setPosition : AntT -> Coords -> AntT
setPosition ant coords' = { ant | position <- coords' }