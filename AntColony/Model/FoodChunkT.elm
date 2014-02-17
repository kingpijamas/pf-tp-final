module AntColony.Model.FoodChunkT where

import open AntColony.Model.Food
import open AntColony.Model.FoodCarrier

type FoodChunkT = FoodCarrier {}

foodChunk : Food -> FoodChunkT
foodChunk food = { food = Just food
                 , limit = Nothing
                 }