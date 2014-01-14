module Model.Ant.Ant where

import open Model.Food
import Geography.Direction as D

type Cargo = FoodCarrier {}

type Ant = { cargo : Cargo
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