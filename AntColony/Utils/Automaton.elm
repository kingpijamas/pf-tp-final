module AntColony.Utils.SignalFunction where

import open AntColony.Utils.SignalFunction
import open AntColony.Utils.MaybeMonad

impure:(a->Maybe b) -> SF (Maybe a) (Maybe b)
impure f = arr (\mbx -> mbx >>= f)

{-- check if this is ok! --}
first' : b -> (a->b->b) -> SF a (a,b)
first' def f = arr (\input -> (input, f' input def))

{-- check if this is ok! --}
first' : b -> (a->b->b) -> SF a (a,b)
first' def f = arr (\input -> (input, f' input def))


--(&&&) :  SF a b -> SF a b' -> SF a (b, b')
--(&&&) a1 a2 = (run a1 Nothing, run a2 Nothing) 
---- run : SF a b -> b -> Signal a -> Signal b
---- a1  : SF a b
-----------------------------------------------------
---- (run a1) : b -> Signal a -> Signal b

---- (run a2) : b' -> Signal a -> Signal b'


{-- check if this is ok! --}
switch2' : b -> (a -> b -> b) -> c -> (a -> c -> c) -> SF a (b,c)
switch2' def1 f1 def2 f2 = arr (\input -> (f1 input def1, f2 input def2))

switch2 : (a -> b) -> (a -> Maybe(c) -> Maybe(c)) -> SF a (b, Maybe(c))
switch2 f1 f2 = switch2' Nothing (\input _ -> input) Nothing f2

{-- check if this is ok! --}
switch3' : b -> (a -> b -> b) -> c -> (a -> c -> c) -> d -> (a -> d -> d) -> SF a (b,c,d)
switch3' def1 f1 def2 f2 def3 f3 = arr (\input -> (f1 input def1, f2 input def2))

switch3 : (a -> b) -> (a -> Maybe(c) -> Maybe(c)) -> (a -> Maybe(d) -> Maybe(d)) -> SF a (b, Maybe(c), Maybe(d))
switch3 f1 f2 f3 = switch3' Nothing (\input _ -> input) Nothing f2 Nothing f3

{-- check if this is ok! --}
switch4' : b -> (a -> b -> b)
             -> c -> (a -> c -> c)
             -> d -> (a -> d -> d)
             -> e -> (a -> e -> e)
             -> SF a (b,c,d,e)
switch4' def1 f1 def2 f2 def3 f3 def4 f4 = arr (\input -> ( f1 input def1
                                                           , f2 input def2
                                                           , f3 input def3
                                                           , f4 input def4
                                                           )
                                                )

{-- check if this is ok! --}
switch5' : b -> (a -> b -> b)
             -> c -> (a -> c -> c)
             -> d -> (a -> d -> d)
             -> e -> (a -> e -> e)
             -> f -> (a -> f -> f)
             -> SF a (b,c,d,e,f)
switch5' def1 f1 def2 f2 def3 f3 def4 f4 def5 f5 = arr (\input -> ( f1 input def1
                                                                   , f2 input def2
                                                                   , f3 input def3
                                                                   , f4 input def4
                                                                   , f5 input def5
                                                                   )
                                                        )

{-- check if this is ok! --}
switch6' : b -> (a -> b -> b)
             -> c -> (a -> c -> c)
             -> d -> (a -> d -> d)
             -> e -> (a -> e -> e)
             -> f -> (a -> f -> f)
             -> g -> (a -> g -> g)
             -> SF a (b,c,d,e,f,g)
switch6' def1 f1 def2 f2 def3 f3 def4 f4 def5 f5 def6 def6 = arr (\input -> ( f1 input def1
                                                                             , f2 input def2
                                                                             , f3 input def3
                                                                             , f4 input def4
                                                                             , f5 input def5
                                                                             , f6 input def6
                                                                             )
                                                                  )

{-- check if this is ok! --}
switch6' : b -> (a -> b -> b)
             -> c -> (a -> c -> c)
             -> d -> (a -> d -> d)
             -> e -> (a -> e -> e)
             -> f -> (a -> f -> f)
             -> g -> (a -> g -> g)
             -> SF a (b,c,d,e,f,g)
switch6' def1 f1 def2 f2 def3 f3 def4 f4 def5 f5 def6 def6 = arr (\input -> ( f1 input def1
                                                                             , f2 input def2
                                                                             , f3 input def3
                                                                             , f4 input def4
                                                                             , f5 input def5
                                                                             , f6 input def6
                                                                             )
                                                                  )




--TODO head: (a->Maybe b) -> (Maybe b -> Maybe c) -> A.SF a Maybe c 
--TODO body: SF a (Maybe b) -> (Maybe b -> Maybe c) -> A.SF a (Maybe c)

--(==>) = head
--(>==>) = body