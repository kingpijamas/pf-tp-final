module Capacities.Seeing where

import Geography.Area as A
import Capacities.Perceiving as P

data Visible a b = Obstacle a | Other b
type Watchable a b c = { c | visibleContent:Maybe(Visible a b) }

see : P.PerceptionF (Watchable a b c) (Visible a b)
see watchable = watchable.visibleContent

type Watcher a b c = P.Perceiver (Watchable a b c) (Visible a b)

watcher : A.Area(Watchable a b c) -> (Watcher a b c)
watcher area = P.perceiver area see