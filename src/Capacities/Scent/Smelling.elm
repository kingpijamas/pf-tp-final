module Capacities.Scent.Smelling where

import Either as E
import Geography.Area as A
import Capacities.Perceiving as P
import Capacities.Scent.Scent as Sc


type Smellable a = Sc.Scentable a
type Smell = Sc.Scent

smell : P.PerceptionF (Smellable a) (Smell)
smell smellable = if (smellable.scent == 0)
                    then Nothing
                    else Just smellable.scent

type Smeller a = P.Perceiver (Smellable a) (Smell)

smeller : A.Area(Smellable a) -> (Smeller a)
smeller area = P.perceiver area smell