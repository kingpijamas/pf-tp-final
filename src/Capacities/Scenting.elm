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

scent:LocationSignal' a -> Maybe(Area' a)
scent sig = let area = (sig.who).area
                targetPos = sig.target

                scent' target = A.add area targetPos { target | scent <- (target.scent)+1 }

             in
                (area `A.get` targetPos) -- : Maybe (Scentable a)
                 >>= (scent')            -- : Scentable a -> Maybe (Area' a)

unscent:LocationSignal' a -> Maybe(Area' a)
unscent sig = let area = (sig.who).area
                  targetPos = sig.target

                  scent' target = A.add area targetPos { target | scent <- (target.scent)-1 }
               in
                  (area `A.get` targetPos) -- : Maybe (Scentable a)
                   >>= (scent')            -- : Scentable a -> Maybe (Area' a)

scentProxy:ScentSignal a -> Maybe(Area' a)
scentProxy sig = let sig' = A.locationSignal (sig.who) (sig.target)
                 in
                   case sig.scentAction of
                      Scent -> scent sig'
                      Unscent -> unscent sig'

type Scenter a = Auto.Automaton (ScentSignal a) (Maybe(Area' a))

scenter:Scenter a
scenter = Auto.pure(scentProxy)