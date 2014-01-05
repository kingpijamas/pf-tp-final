module Capacities.Scent.Scenting where

import Geography.Area as A
import Automaton as Auto
import Utils.MaybeMonad as M
import Capacities.Scent.Scent as Sc

(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

type Area' a = A.Area (Sc.Scentable a)

data ScentActions = Scent | Unscent
type ScentSignal a = { target:A.Coords
                     , action:ScentActions
                     }

type ScentF a = (Sc.Scentable a -> Sc.Scentable a)

scentUnscent : ScentF a -> ScentF a -> Area' a -> ScentSignal a -> Maybe(Area' a)
scentUnscent scent unscent area sig = let tPos = sig.target

                                          scentUnscent' t = M.return ( case sig.action of
                                                                            Scent ->  scent t
                                                                            Unscent -> unscent t
                                                                        )

                                          updateArea t' = A.add area tPos t'
                                       in
                                          (area `A.get` tPos)       -- Maybe(Scentable a)
                                           >>= (scentUnscent')      -- Scentable a -> Maybe (Scentable a)
                                           >>= (updateArea)         -- Scentable a -> Maybe (Area' a)
                             

type Scenter a = Auto.Automaton (ScentSignal a) (Maybe(Area' a))

scenter : ScentF a -> ScentF a -> Area' a -> Scenter a
scenter scent unscent area = Auto.pure(scentUnscent scent unscent area)