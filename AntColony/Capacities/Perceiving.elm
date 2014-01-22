module AntColony.Capacities.Perceiving where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction
import open AntColony.Utils.MaybeMonad
import open Automaton
import open AntColony.Capacities.AreaSignals

type PerceptionSignal p = { perceived:p
                          , location:Coords
                          }

perceptionSignal : Coords -> p -> PerceptionSignal p
perceptionSignal location perceived = { perceived = perceived
                                      , location = location
                                      }

type PerceptionF a p = a -> Maybe p

perceive : (PerceptionF a p) -> (Area a) -> LocationSignal -> (Maybe(PerceptionSignal p))
perceive pf area sig = let targetPos = sig.target
                           perceptionSignal' location = return (perceptionSignal targetPos location)
                        in
                          (area `get` targetPos)        -- : Maybe (a)
                            >>= (pf)                    -- : a -> Maybe(p)
                            >>= (perceptionSignal')     -- : p -> Maybe(PerceptionSignal p)

{-- Does the a make any sense here? --}
type Perceiver a p = Automaton (LocationSignal) (Maybe(PerceptionSignal p))

perceiver : (PerceptionF a p) -> (Area a) -> (Perceiver a p)
perceiver pf area = pure (perceive pf area)

perceiveInDir : Direction -> (PerceptionF a p) -> (Area a) -> LocationSignal -> (Maybe(PerceptionSignal p))
perceiveInDir dir pf area lSig = let asLSig targetP = return (locationSignal (lSig.from) targetP)
                                     
                                     perceive' = perceive pf area
								  in
								  	 (lSig.target `addDir` dir)		-- : Maybe(Coords)
								  	  >>= (asLSig)					-- : Coords -> Maybe(LocationSignal)
								  	  >>= (perceive')				-- : LocationSignal -> Maybe(PerceptionSignal p)