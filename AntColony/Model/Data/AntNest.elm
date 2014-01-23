module AntColony.Model.Data.AntNest where

import open AntColony.Model.Data.Food

type AntNest = FoodCarrier {}

antNest : AntNest
antNest = { food = Nothing
          , limit = Nothing
          }