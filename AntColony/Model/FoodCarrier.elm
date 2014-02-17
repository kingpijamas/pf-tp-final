module AntColony.Model.FoodCarrier where

import open AntColony.Model.Food
import open AntColony.Utils.Maybe

type FoodCarrier a = { a | food:Maybe(Food), limit:Maybe(Int) }

data LoadStatus = Empty | Full

getStatus : (FoodCarrier a) -> LoadStatus
getStatus carrier = case (carrier.limit, carrier.food) of
                         (Nothing, _) -> Empty
                         (Just lmt, Nothing) -> Empty
                         (Just lmt, Just cargo) -> if cargo < lmt
                                                   then Empty
                                                   else Full


load : (FoodCarrier a) -> Food -> Maybe (FoodCarrier a, Maybe(Food))
load ldr fd = let assertValid fd = food fd

                  nothingAsZero mbx = case mbx of
                                               Nothing -> 0
                                               Just x -> x

                  loadAndLimit = return (nothingAsZero (ldr.food), nothingAsZero (ldr.limit))
      
                  separateLoad (ldrFood, lmt) = if (ldrFood + fd) < lmt
                                                then return (food (ldrFood + fd), food 0)
                                                else return (food lmt, food (fd - lmt))

                  load' (ld,rem) = return ({ldr | food <- ld}, rem)
               in 
                  (assertValid fd)                    -- : Maybe(Food)
                   >> (loadAndLimit)                  -- : Maybe(Food, Int)
                   >>= (separateLoad)                 -- : (Food,Int) -> Maybe(Maybe(Food),Maybe(Food))
                   >>= (load')                        -- : (Maybe(Food),Maybe(Food)) -> Maybe(FoodCarrier a,Maybe(Food))

unload : (FoodCarrier a) -> Maybe (FoodCarrier a, Food)
unload unldr = let empty food = return ({ unldr | food <- Nothing }, food)
                in 
                   (unldr.food)  -- : Maybe(Food)
                    >>= (empty)  -- : Food -> Maybe(FoodCarrier a, Food)