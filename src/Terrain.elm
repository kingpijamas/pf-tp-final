module Terrain where
import Geography.Area as A

import Either as E

import Capacities.Moving as Mv
import Capacities.Loading as Ld
import Capacities.Scenting as Sc
import Capacities.Perceiving as P
import Capacities.Seeing as See
import Capacities.Smelling as Sm

type Terrain = A.Area Tile

--TODO
type Food = Int
--TODO 
type Ant = Int
--TODO
type Rock = Int
--TODO
type Pheromone = Int

--FIXME: link Scentable and Occupiable (what about Carrying?) with the elements in Watchable and Smellable






type Tile = Mv.Occupiable ( 
                Ld.Carrying (
                    Sc.Scentable (
                        See.Watchable (Rock) (Ant) (
                            Sm.Smellable (Food) (Pheromone) (
                                A.Locatable {}
                            )
                        )
                    )
                )
            )

-- Well, as expected, instancing these is a problem
pepe : Tile 
pepe = {}
--pepe = { occupiation = Mv.Occupied
--       , cargo = Ld.Empty
--       , scent = 0
--       , visibleContent = Just (See.Obstacle(100))
--       , smellingContent = Just (E.Left(200))
--       , location = (A.coords 0 0)
--       }

            --{ solid:Either Moving Rigid
   --         , scent:Maybe Smellable
   --         }

--type Smellable = Either Pheromone Food