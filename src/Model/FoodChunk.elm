module Model.FoodChunk where
import open Model.Food

type FoodChunk = FoodCarrier {}

foodChunk : Food -> FoodChunk
foodChunk food = { food = Just food
                 , limit = Nothing
                 }