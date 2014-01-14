module Model.Ant.Brain where

import open Automaton

import Ant.Seeing as See
import Ant.Smelling as Sm
import Ant.Moving as Mv
import Ant.Loading as Ld
import Ant.Scenting as Sc


brain  Sc.smeller 


type Brain = Automaton Ant Ant