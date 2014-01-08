module Capacities.Seeing where

import open Geography.Area
import open Capacities.Perceiving

data Visible a b = Obstacle a | Other b
type Watchable a b c = { c | visibleContent:Maybe(Visible a b) }

see : PerceptionF (Watchable a b c) (Visible a b)
see watchable = watchable.visibleContent

type Watcher a b c = Perceiver (Watchable a b c) (Visible a b)

watcher : Area(Watchable a b c) -> (Watcher a b c)
watcher area = perceiver area see