module AntColony.Utils.Automaton where
import open Automaton
import open AntColony.Utils.MaybeMonad

impure:(a->Maybe b) -> Automaton (Maybe a) (Maybe b)
impure f = pure (\mbx -> mbx >>= f)

{-- check if this is ok! --}
first' : b -> (a->b->b) -> Automaton a (a,b)
first' def f = let f' input def = (input, f' input def)
                in
                   pure f'

{-- check if this is ok! --}
switch2' : b -> (a -> b -> b) -> c -> (a -> c -> c) -> Automaton a (b,c)
switch2' def1 f1 def2 f2 = let f2' (input, o1) = (o1, f2 input def2)     -- : (a,b) -> (b,c)
                            in
                               (first' def1 f1')     -- : Automaton a (a,b)
                                >>> (pure f2')       -- : Automaton (a,b) (b,c)

switch2 : (a -> b) -> (a -> Maybe(c) -> Maybe(c)) -> Automaton a (b, Maybe(c))
switch2 f1 f2 = switch2' Nothing (\input _ -> input) Nothing f2

{-- check if this is ok! --}
switch3' : b -> (a -> b -> b) -> c -> (a -> c -> c) -> d -> (a -> d -> d) -> Automaton a (b,c,d)
switch3' def1 f1 def2 f2 def3 f3 = let f2' (input, o1) = (input, o1, f2 input def2)    -- : (a,b) -> (a,b,c)
                                       f3' (input, o1, o2) = (o1, o2, f3 input def3)   -- : (a,b,c) -> (b,c,d)
                                    in
                                       (first' def1 f1)      -- : Automaton a (a,b)
                                        >>> (pure f2')       -- : Automaton (a,b) (a,b,c)
                                        >>> (pure f3')       -- : Automaton (a,b,c) (b,c,d)

switch3 : (a -> b) -> (a -> Maybe(c) -> Maybe(c)) -> (a -> Maybe(d) -> Maybe(d)) -> Automaton a (b, Maybe(c), Maybe(d))
switch3 f1 f2 f3 = switch3' Nothing (\input _ -> input) Nothing f2 Nothing f3


--TODO head: (a->Maybe b) -> (Maybe b -> Maybe c) -> A.Automaton a Maybe c 
--TODO body: Automaton a (Maybe b) -> (Maybe b -> Maybe c) -> A.Automaton a (Maybe c)

--(==>) = head
--(>==>) = body