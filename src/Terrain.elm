module Terrain where
import Area.elm as Area

type Terrain = Area.Area Tile

type Tile = { solid:Either Moving Rigid
            , scent:Maybe Smellable
            }

type Smellable = Either Pheromone Food








{-| pun not intended
    -}
--(+) : Location -> Location -> Location
--(+) {r1,c1} {r2,c2} = {r1+r2, c1+c2}

--(-) : Location -> Location -> Location
--(-) {r1,c1} {r2,c2} = {r1-r2, c1-c2}

