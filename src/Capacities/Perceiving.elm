module Capacities.Perceiving where

import open Geography.Area
import open Utils.MaybeMonad
import open Automaton

type PerceptionSignal p = { perceived:p
                          , location:Coords
                          }

perceptionSignal : Coords -> p -> PerceptionSignal p
perceptionSignal location perceived = { perceived=perceived, location=location }

type PerceptionF a p = (a -> Maybe p)

perceive :(PerceptionF a p) -> (Area a) -> LocationSignal -> (Maybe(PerceptionSignal p))
perceive pf area sig = let targetPos = sig.target
                           perceptionSignal' location = return (perceptionSignal targetPos location)
                        in
                          (area `get` targetPos)        -- : Maybe (a)
                            >>= (pf)                    -- : a -> Maybe(p)
                            >>= (perceptionSignal')     -- : p -> Maybe(PerceptionSignal p)

type Perceiver a p = Automaton (LocationSignal) (Maybe(PerceptionSignal p))

perceiver : (PerceptionF a p) -> (Area a) -> (Perceiver a p)
perceiver pf area = pure (perceive pf area)