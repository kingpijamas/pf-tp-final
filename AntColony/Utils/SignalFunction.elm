module AntColony.Utils.SignalFunction where

import open Signal
import open AntColony.Utils.Tuple

data SF a b = SF (a -> b) (Signal a -> Signal b)

arr : (a -> b) -> SF a b
arr f = SF f (lift f)

(>>>) : SF a b -> SF b c -> SF a c
(SF f1 _) >>> (SF f2 _) = arr (f2 . f1)

(<<<) : SF b c -> SF a b -> SF a c
sf1 <<< sf2 = sf2 >>> sf1

--(&) : (a -> b) -> (a -> c) -> a -> (b,c)
--f1 & f2 = \x -> (f1 x, f2 x)

(&&&) : SF a b -> SF a c -> SF a (b,c)
(SF f1 _) &&& (SF f2 _) = arr (f1 & f2)

(***) : SF b c -> SF b' c' -> SF (b, b') (c, c')
f *** g = (arr fst >>> f) &&& (arr snd >>> g)

identity : SF a a
identity = arr id

first : SF b c -> SF (b,d) (c,d)
first sf1 = sf1 *** identity

second : SF b c -> SF (d,b) (d,c)
second sf1 = identity *** sf1