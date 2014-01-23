module AntColony.Utils.MaybeMonad where

return : a -> Maybe a
return x = Just x

(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
mbx >>= f = case mbx of
                Just x -> f x
                _   -> Nothing

--join : Maybe (Maybe a) -> Maybe a
--join mm = mm 						-- : Maybe(Maybe a) 
--		   >>= (id)					-- : Maybe a -> Maybe a

--TODO: get real name for this one!
(>>) : Maybe a -> Maybe b -> Maybe b
x >> y = x >>= (\_ -> y)