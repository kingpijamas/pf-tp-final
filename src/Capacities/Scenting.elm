module Capacities.Scenting where

import Geography.Area as A
import Automaton as Auto
import Utils.MaybeMonad as M


(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

type Scent = Int
type Scentable a = A.Locatable { a | scent:Scent }

type LocationSignal' a = A.LocationSignal (Scentable a)
type Area' a = A.Area (Scentable a)

data ScentActions = Scent | Unscent
type ScentSignal a = { who:A.Locatable (Scentable a)
                     , target:A.Coords
                     , scentAction:ScentActions
                     }


scentUnscent : Area' a -> ScentSignal a -> Maybe(Area' a)
scentUnscent area sig = let scentUnscent' target = M.return (case sig.scentAction of
                                                                Scent -> scent target
                                                                Unscent -> unscent target)

                            scent target = { target | scent <- (target.sent)+1 }
                            unscent target = { target | scent <- (target.sent)-1 }

                            updateArea target' = A.add area (target'.location) target'
                         in
                            (area `A.get` sig.target) 
                             >>= (scentUnscent')
                             >>= (updateArea)
                             

type Scenter a = Auto.Automaton (ScentSignal a) (Maybe(Area' a))

scenter : Area' a -> Scenter a
scenter area = Auto.pure(scentUnscent area)