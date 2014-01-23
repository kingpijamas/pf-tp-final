module AntColony.Model.Data.FoodChunk where

import open AntColony.Model.Data.Food

type FoodChunk = FoodCarrier {}

foodChunk : Food -> FoodChunk
foodChunk food = { food = Just food
                 , limit = Nothing
                 }