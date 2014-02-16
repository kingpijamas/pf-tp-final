module AntColony.Logic.Perceiving where

import open AntColony.Model.Terrain

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import open AntColony.Utils.Maybe
import open AntColony.Utils.SignalFunction

type PerceptionF p = (Position -> Maybe p)

perceive : PerceptionF p -> Terrain -> Coords -> Maybe(p)
perceive pf terrain targetPos = (terrain `get` targetPos)    -- : Maybe(Position)
                                 >>= pf                      -- : Position -> Maybe(p)

perceiveInDir : PerceptionF p -> Direction -> Terrain -> Coords -> Maybe(p)
perceiveInDir pf dir terrain from = (from `addDir` dir)            -- : Maybe(Coords)
                                     >>= (perceive pf terrain)     -- : Coords -> Maybe(Perception p)

perceiveInDirs : PerceptionF p -> [Direction] -> Terrain -> Coords -> [Maybe(p)]
perceiveInDirs pf dirs terrain from = map (\dir -> perceiveInDir pf dir terrain from) dirs

-- perceptor : PerceptionF p -> SF (Terrain, Coords) (Maybe p)
-- perceptor pf = arr (uncurry (perceive pf))