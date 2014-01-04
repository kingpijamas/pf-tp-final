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

--IDEA scent:forma de scentear -> A.LocationSignal a -> Maybe(Area' a)
--let the code below serve as a demonstration why this is needed:

scent : Area' a -> LocationSignal' a -> Maybe(Area' a)
scent area sig = let targetPos = sig.target

                     scent' target = A.add area targetPos { target | scent <- (target.scent)+1 }

                  in
                    (area `A.get` targetPos) -- : Maybe (Scentable a)
                     >>= (scent')            -- : Scentable a -> Maybe (Area' a)

unscent : Area' a -> LocationSignal' a -> Maybe(Area' a)
unscent area sig = let targetPos = sig.target

                       scent' target = A.add area targetPos { target | scent <- (target.scent)-1 }
                    in
                      (area `A.get` targetPos)  -- : Maybe (Scentable a)
                        >>= (scent')            -- : Scentable a -> Maybe (Area' a)

scentProxy : Area' a -> ScentSignal a -> Maybe(Area' a)
scentProxy area sig = let sig' = A.locationSignal (sig.who) (sig.target)
                       in
                         case sig.scentAction of
                            Scent -> scent area sig'
                            Unscent -> unscent area sig'

type Scenter a = Auto.Automaton (ScentSignal a) (Maybe(Area' a))

scenter : Area' a -> Scenter a
scenter area = Auto.pure(scentProxy area)