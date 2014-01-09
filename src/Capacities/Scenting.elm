module Capacities.Scent.Scenting where

import open Geography.Area
import open Automaton
import open Utils.MaybeMonad

type ScentF a = Area a -> Coords -> Maybe(Area a)
type UnscentF a = ScentF a

data Action = Scent | Unscent

type ScentSignal = { target:Coords
                   , action:Action
                   }

scentUnscent : ScentF a -> UnscentF a -> Area a -> ScentSignal -> Maybe(Area a)
scentUnscent scent unscent area sig = let targetPos = sig.target

                                          scentUnscent' t = case sig.action of
                                                                Scent ->  scent t
                                                                Unscent -> unscent t
                                       in
                                          scentUnscent' area targetPos  -- Maybe(Area a)
                             

type Scenter a = Automaton (ScentSignal) (Maybe(Area a))

scenter : ScentF a -> ScentF a -> Area a -> Scenter a
scenter scent unscent area = pure(scentUnscent scent unscent area)