module Capacities.Moving where

import Geography.Area as A
import Geography.DirectionUtils as D
import Automaton as Auto
import Utils.MaybeMonad as M
import Utils.AutomatonExt as Ext


(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}
(>>>) = (>>>) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

type Occupiable a b = { b | occupant:(Maybe a) }

type Area' a b = A.Area (Occupiable a b)

type MovementSignal a b = { who:a
                          , from:A.Coords
                          , target:A.Coords
                          }

mv : (Area' a b) -> (MovementSignal a b) -> Maybe (Area' a b)
mv area sig = let toMv = sig.who
                  from = sig.from
                  to = sig.target

                  getFromPos = area `A.get` from

                  getTargetPos fPos = (fPos, area `A.get` to)

                  mv' fPos tPos = case (tPos.occupant) of
                                    Nothing -> M.return ({fPos | occupant <- Nothing},{tPos | occupant <- toMv})
                                    _       -> Nothing

                  updateFrom (fPos', tPos') = (A.add area from fPos', tPos')

                  updateTo (area', tPos') = A.add area' to tPos'
               in
                  getFromPos              -- : Maybe (Occupiable a b)
                   >>= getTargetPos       -- : Occupiable a b -> Maybe(Occupiable a b, Occupiable a b)
                   >>= mv'                -- : (Occupiable a b, Occupiable a b) -> Maybe (Occupiable a b, Occupiable a b)
                   >>= updateFrom         -- : (Occupiable a b, Occupiable a b) -> Maybe (Area' a b, Occupiable a b)
                   >>= updateTo           -- : (Area' a b, Occupiable a b) -> Maybe(Area' a b)

type Motor a b = Auto.Automaton (D.DirectionalSignal) (Maybe(Area' a b))

motor : (Area' a b) -> (Motor a b)
motor area = Auto.pure(D.toLocSig) >>> Ext.impure(mv area)

--type Moving a = { a | motor:(Motor a) } {--TODO--}