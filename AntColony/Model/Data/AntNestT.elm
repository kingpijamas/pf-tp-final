module AntColony.Model.Data.AntNestT where

import open AntColony.Model.Data.Food

type AntNestT = FoodCarrier {}

antNest : AntNestT
antNest = { food = Nothing
          , limit = Nothing
          }