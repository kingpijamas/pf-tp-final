module Food where
import open Utils.MaybeMonad

type Food = Int

food : Int -> Maybe(Food)
food x = if x > 0
         then Just(x)
         else Nothing

type FoodCarrier a = { a | food:Maybe(Food), limit:Maybe(Int) }

load : (FoodCarrier a) -> Food -> Maybe (FoodCarrier a, Maybe(Food))
load ldr fd = let assertValid fd = if fd > 0
                                   then return fd
                                   else Nothing

                  separateLoad fd = if (ldr.food + fd) < lmt
                                    then (fd, 0)
                                    else (lmt - fd, fd - lmt)

                  load' (ld,rem) = ({ldr | food <- Just(ldr.food + fd)}, food rem)
               in 
                  (assertValid fd)                    -- : Maybe(Food)
                    >>= (return . separateLoad)       -- : Food -> Maybe(Food,Food)
                    >>= (return . load')              -- : (Food,Food) -> Maybe(Food,Maybe(Food))

unload : (FoodCarrier a) -> Maybe (FoodCarrier a, Food)
unload unldr = let empty food = return ({ unldr | food <- Nothing }, food)
                in 
                  (unldr.food)  -- : Maybe(Food)
                   >>= (empty)  -- : Food -> Maybe(FoodCarrier a, Food)