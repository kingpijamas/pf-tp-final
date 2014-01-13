module Model.AntNest where
import open Model.Food

type AntNest = FoodCarrier {}

antNest : AntNest
antNest = { food = Nothing
          , limit = Nothing
          }