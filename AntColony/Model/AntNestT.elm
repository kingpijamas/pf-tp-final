module AntColony.Model.AntNestT where

import open AntColony.Model.FoodCarrier

type AntNestT = FoodCarrier {}

antNest : AntNestT
antNest = { food = Nothing
          , limit = Nothing
          }