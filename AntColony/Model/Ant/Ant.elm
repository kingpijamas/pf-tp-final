module Model.Ant.Ant where

import open Model.Food

type AntT = FoodCarrier {}

ant : AntT
ant = { food = Nothing
      , limit = 1
      }