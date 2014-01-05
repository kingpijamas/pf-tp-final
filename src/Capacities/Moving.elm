module Capacities.Moving where

import Geography.Area as A
import Geography.DirectionExt as D
import Automaton as Auto
import Utils.MaybeMonad as M
import Utils.AutomatonExt as Ext


(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}
(>>>) = (>>>) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

data Occupiation = Occupied | Empty
type Occupiable a = { a | occupiation:Occupiation }

type DirectionalSignal' a = D.DirectionalSignal (Occupiable a)
type LocationSignal' a = A.LocationSignal (Occupiable a)
type Area' a = A.Area (Occupiable a)

mv : (Area' a) -> LocationSignal' a -> Maybe (Area' a)
mv area sig = let toMv = sig.who
                  from = toMv.location
                  to = sig.target

                  getTargetPos = area `A.get` to

                  getOccupiation targetPos = M.return (targetPos.occupiation)
                
                  clearIfNotOcc occ = case occ of
                                        Empty -> M.return (area `A.remove` from) {-- FIXME sth smells weird here; wouldn't this be a Maybe(Maybe(Occupiable a))? --}
                                        Occupied -> Nothing

                  mv' area = A.add area to { toMv | location <- to }
               in
                 getTargetPos
                  >>= (getOccupiation)
                  >>= (clearIfNotOcc) {--TODO Maybe not the nicest name --}
                  >>= (mv')

type Motor a = Auto.Automaton (DirectionalSignal' a) (Maybe(Area' a))

motor : Area' a -> Motor a
motor area = Auto.pure(D.toLocSig) >>> Ext.impure(mv area)

--type Moving a = { a | motor:(Motor a) } {--TODO--}