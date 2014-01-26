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
                                    (terrain `get` targetPos)    -- : Maybe (a)
                                     >>= (pf)                    -- : a -> Maybe(p)
                                     >>= (perception')           -- : p -> Maybe(Perception p)

{-- Does the a make any sense here? --}
{--type Perceiver p = SF (LocationIntent) (Maybe(Perception p))

perceiver : PerceptionF p -> Terrain -> Perceiver p
perceiver pf terrain = arr (perceive pf terrain)

perceiveInDir : Direction -> PerceptionF p -> Terrain -> LocationIntent -> Maybe(Perception p)
perceiveInDir dir pf terrain lSig = let asLSig targetP = return (locationIntent (lSig.from) targetP)
                                     
                                        perceive' = perceive pf terrain
                                     in
                                        (lSig.target `addDir` dir)  -- : Maybe(Coords)
                                         >>= (asLSig)               -- : Coords -> Maybe(LocationIntent)
                                         >>= (perceive')            -- : LocationIntent -> Maybe(Perception p)
--}