module Capacities.Smelling where

import Either as E
import Geography.Area as A
import Capacities.Perceiving as P

type Scented a b = E.Either a b  {--TODO A new, more specific type would be cool--}
type Smellable a b c = { c | smellingContent:Maybe(Scented a b) }

smell : P.PerceptionF (Smellable a b c) (Scented a b)
smell smellable = smellable.smellingContent

type Smeller a b c = P.Perceiver (Smellable a b c) (Scented a b)

smeller : A.Area(Smellable a b c) -> (Smeller a b c)
smeller area = P.perceiver area smell