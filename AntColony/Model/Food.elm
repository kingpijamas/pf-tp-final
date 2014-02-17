module AntColony.Model.Food where
  
import open AntColony.Utils.Maybe

type Food = Int

food : Int -> Maybe(Food)
food x = if x > 0
         then Just(x)
         else Nothing