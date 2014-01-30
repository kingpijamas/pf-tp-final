module AntColony.Model.Perceiving where

import open AntColony.Model.Data.Terrain

import open AntColony.Geography.Area
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

perceptor : PerceptionF p -> Terrain -> SF Coords (Maybe(p))
perceptor pf terrain = arr (perceive pf terrain)

perceptors : PerceptionF p -> [Direction] -> Terrain -> SF Coords [Maybe(p)]
perceptors pf sensingDirs terrain = let p dir = arr (perceiveInDir pf dir terrain)
                                     in
                                        combine (map p sensingDirs)