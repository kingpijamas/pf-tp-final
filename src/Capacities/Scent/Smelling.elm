module Capacities.Scent.Smelling where

import open Geography.Area
import open Capacities.Perceiving
import Capacities.Scent.Scent as Sc


type Smellable a = Sc.Scentable a
type Smell = Sc.Scent

smell : PerceptionF (Smellable a) (Smell)
smell smellable = if (smellable.scent == 0)
                    then Nothing
                    else Just smellable.scent

type Smeller a = Perceiver (Smellable a) (Smell)

smeller : Area(Smellable a) -> (Smeller a)
smeller area = perceiver area smell