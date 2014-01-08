module Capacities.Moving where

import open Geography.Area
import open Geography.DirectionUtils
import open Automaton
import open Utils.MaybeMonad
import open Utils.AutomatonUtils

type Occupiable a b = { b | occupant:(Maybe a) }

type Area' a b = Area (Occupiable a b)

mv : (Area' a b) -> LocationSignal -> Maybe (Area' a b)
mv area sig = let from = sig.from
                  to = sig.target

                  getFromPos = area `get` from

                  getTargetPos fPos = case (area `get` to) of
                                            Just tPos -> return ((fPos,tPos))
                                            _ -> Nothing

                  mv' (fPos,tPos) = case (tPos.occupant) of
                                        Nothing -> return ({fPos | occupant <- Nothing},{tPos | occupant <- fPos.occupant})
                                        _       -> Nothing

                  updateFrom (fPos', tPos') = case (add area from fPos') of
                                                Just area' -> return ((area',tPos'))
                                                _          -> Nothing

                  updateTo (area', tPos') = add area' to tPos'
               in
                  getFromPos              -- : Maybe (Occupiable a b)
                   >>= getTargetPos       -- : Occupiable a b -> Maybe(Occupiable a b, Occupiable a b)
                   >>= mv'                -- : (Occupiable a b, Occupiable a b) -> Maybe (Occupiable a b, Occupiable a b)
                   >>= updateFrom         -- : (Occupiable a b, Occupiable a b) -> Maybe (Area' a b, Occupiable a b)
                   >>= updateTo           -- : (Area' a b, Occupiable a b) -> Maybe(Area' a b)

type Motor a b = Automaton (DirectionalSignal) (Maybe(Area' a b))

motor : (Area' a b) -> (Motor a b)
motor area = pure(toLocSig) >>> impure(mv area)

--type Moving a = { a | motor:(Motor a) } {--TODO--}