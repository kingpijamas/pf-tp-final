module AntColony.Model.AntNestT where

import open AntColony.Model.Food

type AntNestT = FoodCarrier {}

antNest : AntNestT
antNest = { food = Nothing
          , limit = Nothing
          }