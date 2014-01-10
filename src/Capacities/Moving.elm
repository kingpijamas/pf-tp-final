module Capacities.Moving where

import open Geography.Area
import open Geography.DirectionUtils
import open Automaton
import open Utils.MaybeMonad
import open Utils.AutomatonUtils

type OccupiationF a = (Area a) -> Coords -> a -> Maybe (Area a)

type EvictionF a = (Area a) -> Coords -> Maybe ((Area a), a)

mv : (OccupiationF a) -> (EvictionF a) -> (Area a) -> LocationSignal -> Maybe (Area a)
mv occupy evict area sig = let from = sig.from
                               to = sig.target

                               occupyWith (area', mver) = occupy area' to mver
                            in
                               (area `evict` from)                  -- : Maybe (Area a, a)
                                >>= (occupyWith)                    -- : (Area a, a) -> Maybe(Area a)

type Motor a = Automaton (DirectionalSignal) (Maybe(Area a))

motor : (OccupiationF a) -> (EvictionF a) -> (Area a) -> (Motor a)
motor occupy evict area = pure(toLocSig) >>> impure(mv occupy evict area)

--type Moving a = { a | motor:(Motor a) } {--TODO--}