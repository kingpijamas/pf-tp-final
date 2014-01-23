module AntColony.Model.Ant.Ant where

import open AntColony.Model.Food
import AntColony.Geography.Direction as D

type Cargo = FoodCarrier {}

type Ant = { position : Coords
           , cargo : Cargo
           , orientation : D.Direction
           }

ant : Ant
ant = { cargo = { food = Nothing
                , limit = Just 1
                }
      , orientation = D.N
      }

setCargo : Ant -> Cargo -> Ant
setCargo ant cargo' = { ant | cargo <- cargo' }

-- Automaton Terrain Terrain