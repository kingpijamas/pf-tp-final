module Utils.MaybeMonad where

return : a -> Maybe a
return x = Just x

bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind mbx f = case mbx of
                Just x -> f x
                _   -> Nothing

(>>=) = bind


--join : Maybe (Maybe a) -> Maybe a
--join mm = mm 						-- : Maybe(Maybe a) 
--		   >>= (id)					-- : Maybe a -> Maybe a

--(>>) : Maybe a -> Maybe b -> Maybe b
--(>>) x y = x (>>=) (\\_ -> y)


