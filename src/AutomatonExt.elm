module AutomatonExt where
import Automaton as A
import MaybeMonad as Mb

(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

impure:(a->Maybe b) -> A.Automaton (Maybe a) (Maybe b)
impure f = A.pure (\mbx -> mbx >>= f)

--TODO head: (a->Maybe b) -> (Maybe b -> Maybe c) -> A.Automaton a Maybe c 
--TODO body: Automaton a (Maybe b) -> (Maybe b -> Maybe c) -> A.Automaton a (Maybe c)

--(==>) = head
--(>==>) = body