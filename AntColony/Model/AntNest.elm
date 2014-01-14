module AntColony.Model.AntNest where
import open AntColony.Model.Food

type AntNest = FoodCarrier {}

antNest : AntNest
antNest = { food = Nothing
          , limit = Nothing
          }