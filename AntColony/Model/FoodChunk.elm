module AntColony.Model.FoodChunk where
import open AntColony.Model.Food

type FoodChunk = FoodCarrier {}

foodChunk : Food -> FoodChunk
foodChunk food = { food = Just food
                 , limit = Nothing
                 }