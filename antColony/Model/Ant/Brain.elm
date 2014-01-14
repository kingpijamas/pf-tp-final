module Model.Ant.Brain where

import open Automaton

import Ant.Seeing as See
import Ant.Smelling as Sm
import Ant.Moving as Mv
import Ant.Loading as Ld
import Ant.Scenting as Sc

type Perceiver a p = Automaton (LocationSignal) (Maybe(PerceptionSignal p))

think1 : Maybe() -> 