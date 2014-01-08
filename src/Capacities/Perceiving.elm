module Capacities.Perceiving where

import open Geography.Area
import open Utils.MaybeMonad
import open Automaton

type PerceptionSignal a = { perceived:a
                          , location:Coords
                          }

perceptionSignal : Coords -> a -> PerceptionSignal a
perceptionSignal location perceived = { perceived=perceived, location=location }

type PerceptionF a b = (a -> Maybe b)

perceive : (Area a) -> (PerceptionF a b) -> LocationSignal -> (Maybe(PerceptionSignal b))
perceive area pf sig = let perceptionSignal' location = return (perceptionSignal sig.target location)
                        in
                          (area `get` sig.target)       -- : Maybe (a)
                            >>= (pf)                    -- : a -> Maybe(b)
                            >>= (perceptionSignal')     -- : b -> Maybe(PerceptionSignal b)

type Perceiver a b = Automaton (LocationSignal) (Maybe(PerceptionSignal b))

perceiver : (Area a) -> (PerceptionF a b) -> (Perceiver a b)
perceiver area pf = pure (perceive area pf)