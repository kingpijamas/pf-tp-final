module AntColony.Model.Perceiving where

import open AntColony.Model.Data.Terrain

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Utils.MaybeMonad
import open AntColony.Utils.SignalFunction



type Perception p = { perceived : p
                    , location : Coords
                    }

perception : Coords -> p -> Perception p
perception location perceived = { perceived = perceived
                                , location = location
                                }

type PerceptionF p = (Position -> Maybe p)

perceive : PerceptionF p -> Terrain -> Coords -> Maybe(Perception p)
perceive pf terrain targetPos = let perception' location = return (perception targetPos location)
                                 in
                                    (terrain `get` targetPos)    -- : Maybe(Position)
                                     >>= pf                      -- : Position -> Maybe(p)
                                     >>= perception'             -- : p -> Maybe(Perception p)

perceiveInDir : PerceptionF p -> Direction -> Terrain -> Coords -> Maybe(Perception p)
perceiveInDir pf dir terrain from = (from `addDir` dir)            -- : Maybe(Coords)
                                     >>= (perceive pf terrain)     -- : Coords -> Maybe(Perception p)