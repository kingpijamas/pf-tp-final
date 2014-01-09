module Capacities.Scent.Smelling where

import open Geography.Area
import open Capacities.Perceiving



smell : PerceptionF (Smellable a) (Smell)
smell smellable = if (smellable.scent == 0)
                    then Nothing
                    else Just smellable.scent

type Smeller a = Perceiver (Smellable a) (Smell)

smeller : Area(Smellable a) -> (Smeller a)
smeller area = perceiver area smell