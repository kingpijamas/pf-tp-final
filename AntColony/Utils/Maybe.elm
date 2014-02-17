module AntColony.Utils.Maybe where

return : a -> Maybe a
return x = Just x

(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
mbx >>= f = case mbx of
                Just x -> f x
                _   -> Nothing

fmap : (a -> b) -> Maybe a -> Maybe b
fmap f mba = mba >>= (return . f)

(<$>) : (a -> b) -> Maybe a -> Maybe b
(<$>) = fmap

(>>=^) : Maybe a -> (a -> b) -> Maybe b
(>>=^) = flip fmap 

--TODO: get real name for this one!
(>>) : Maybe a -> Maybe b -> Maybe b
x >> y = x >>= (\_ -> y)