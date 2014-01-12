module Utils.AutomatonUtils where
import open Automaton
import open Utils.MaybeMonad

impure:(a->Maybe b) -> Automaton (Maybe a) (Maybe b)
impure f = pure (\mbx -> mbx >>= f)





--TODO head: (a->Maybe b) -> (Maybe b -> Maybe c) -> A.Automaton a Maybe c 
--TODO body: Automaton a (Maybe b) -> (Maybe b -> Maybe c) -> A.Automaton a (Maybe c)

--(==>) = head
--(>==>) = body