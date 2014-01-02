module Moving where
import Area as A
import Automaton as Auto
import MaybeMonad as M
import AutomatonExt as Ext

(>>=)=(>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}
(>>>)=(>>>) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

type Position a = { a | occupied:Bool }

type DirectionalSignal' a = A.DirectionalSignal (Position a)
type LocationSignal' a = A.LocationSignal (Position a)
type Area' a = A.Area (Position a)  

mv : LocationSignal' a -> Maybe (Area' a)
mv sig = let 
             toMv = sig.who
             area = toMv.area
             from = toMv.location
             to = sig.target

             getTargetPos = area `A.get` to

             getOccupiation targetPos = M.return (targetPos.occupied)
                
             clearIfNotOcc occ = if not occ
                                   then M.return (area `A.remove` from) {-- FIXME sth smells weird here --}
                                   else Nothing

             setPos toMv to = { toMv | location <- to }

             mv' area = A.add area to (toMv `setPos` to)
          in
             getTargetPos
              >>= (getOccupiation)
              >>= (clearIfNotOcc) {--TODO Maybe not the nicest name --}
              >>= (mv')

type Motor a = Auto.Automaton (DirectionalSignal' a) Maybe(Area' a)

motor : Motor a
motor = Auto.pure(A.toLocSig) >>> Ext.impure(mv)

type Moving a = { a | motor:(Motor a) } {--TODO--}