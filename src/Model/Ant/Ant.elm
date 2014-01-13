module Model.Ant.Ant where

import open Model.Food

type Ant = FoodCarrier {}

ant : Ant
ant = { food = Nothing
      , limit = Just 1
      }