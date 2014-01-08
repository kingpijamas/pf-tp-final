module Terrain where
import Geography.Area as A

import Either as E

import Capacities.Moving as Mv
import Capacities.Cargo.Cargo as Ld
import Capacities.Scent.Scent as Sc
import Capacities.Seeing as See

type Terrain = A.Area Tile

--TODO (Dummy just to see)
type Food = (Int,Int,Int)
--TODO (Dummy just to see)
type Ant = String
--TODO (Dummy just to see)
type Rock = (Int,Int)

--FIXME: link Scentable and Occupiable (what about Carrying?) with the elements in Watchable and Smellable
--This below seems right, the constructor

type Tile = Sc.Scentable (
                See.Watchable (Ant) (Rock) (
                    Mv.Occupiable (E.Either Ant Rock) (
                        Ld.Carrying (Food) {}
                    )
                )
            )

pepe : Tile 
pepe = { scent = 0
        ,visibleContent = Just(See.Obstacle("ant"))
        ,occupant = Just(E.Left("ant"))
        ,cargo = Just((2,3,5))
       }