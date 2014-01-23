module AntColony.Capacities.Scenting where

import open AntColony.Utils.SignalFunction
import open AntColony.Geography.Area
import open AntColony.Utils.MaybeMonad

type ScentF a = Area a -> Coords -> Maybe(Area a)
type UnscentF a = ScentF a

data Action = Scent | Unscent

type ScentSignal = { target:Coords
                   , action:Action
                   }

scentProxy : ScentF a -> UnscentF a -> Area a -> ScentSignal -> Maybe(Area a)
scentProxy scent unscent area sig = let targetPos = sig.target

                                        scentUnscent' t = case sig.action of
                                                                Scent ->  scent t
                                                                Unscent -> unscent t
                                     in
                                        scentUnscent' area targetPos  -- Maybe(Area a)
                             

type Scenter a = SF (ScentSignal) (Maybe(Area a))

scenter : ScentF a -> UnscentF a -> Area a -> Scenter a
scenter scent unscent area = arr (scentProxy scent unscent area)