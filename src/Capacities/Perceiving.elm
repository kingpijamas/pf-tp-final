module Capacities.Perceiving where

import Geography.Area as A
import Utils.MaybeMonad as M
import Automaton as Auto

(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

type PerceptionSignal a = { perceived:a
                          , location:A.Coords
                          }

perceptionSignal : A.Coords -> a -> PerceptionSignal a
perceptionSignal location perceived = { perceived=perceived, location=location }

type PerceptionF a b = (a -> Maybe b)

perceive : (A.Area a) -> (PerceptionF a b) -> (A.LocationSignal a) -> (Maybe(PerceptionSignal b))
perceive area pf sig = let perceptionSignal' = (perceptionSignal sig.target)
                        in
                          (area `A.get` sig.target)
                            >>= (pf)
                            >>= (perceptionSignal')

type Perceiver a b = Auto.Automaton (A.LocationSignal a) (Maybe(PerceptionSignal b))

perceiver : (A.Area a) -> (PerceptionF a b) -> (Perceiver a b)
perceiver area pf = Auto.pure (perceive area pf)