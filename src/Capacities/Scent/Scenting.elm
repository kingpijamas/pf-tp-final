module Capacities.Scent.Scenting where

import open Geography.Area
import open Automaton
import open Utils.MaybeMonad
import Capacities.Scent.Scent as Sc

type Area' a = Area (Sc.Scentable a)

data Action = Scent | Unscent
type ScentSignal a = { a | target:Coords, action:Action }

scentUnscent : Sc.ScentF a -> Sc.ScentF a -> Area' a -> ScentSignal a -> Maybe(Area' a)
scentUnscent scent unscent area sig = let tPos = sig.target

                                          scentUnscent' t = return ( case sig.action of
                                                                            Scent ->  scent t
                                                                            Unscent -> unscent t
                                                                        )

                                          updateArea t' = add area tPos t'
                                       in
                                          (area `get` tPos)       -- Maybe(Scentable a)
                                           >>= (scentUnscent')      -- Scentable a -> Maybe (Scentable a)
                                           >>= (updateArea)         -- Scentable a -> Maybe (Area' a)
                             

type Scenter a = Automaton (ScentSignal a) (Maybe(Area' a))

scenter : Sc.ScentF a -> Sc.ScentF a -> Area' a -> Scenter a
scenter scent unscent area = pure(scentUnscent scent unscent area)