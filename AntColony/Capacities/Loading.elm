module AntColony.Capacities.Loading where

import open AntColony.Geography.Area
import open AntColony.Utils.SignalFunction
import open AntColony.Utils.MaybeMonad
import open AntColony.Capacities.AreaSignals

data LoadAction = Load | Unload

type LoadSignal  = { from:Coords
                   , target:Coords
                   , action:LoadAction
                   }

asLoadSignal : LoadAction -> Coords -> Coords -> LoadSignal
asLoadSignal action from target = {from=from, target=target, action=action}

loadSignal : Coords -> Coords -> LoadSignal
loadSignal = asLoadSignal Load

unloadSignal : Coords -> Coords -> LoadSignal
unloadSignal = asLoadSignal Unload

asLocationSignal : LoadSignal -> LocationSignal
asLocationSignal sig = locationSignal (sig.from) (sig.target)


type LoadF a c = (Area a) -> Coords -> c -> Maybe(Area a, Maybe(c))

type UnloadF a c = (Area a) -> Coords -> Maybe(Area a, c)


load : (LoadF a c) -> (UnloadF a c) -> (Area a) -> LocationSignal -> Maybe(Area a)
load ld unld area sig = let ldrPos = sig.from
                            unldrPos = sig.target

                            load' ldrPos (area',cargo) = ld area' ldrPos cargo

                            returnRemnant unldrPos (area'',rem) = case rem of
                                                                    Just rem' -> load' unldrPos (area'',rem')
                                                                    _ -> return (area'',Nothing)
                         in
                            (area `unld` unldrPos)                 -- : Maybe(Area a,c)
                             >>= (load' ldrPos)                    -- : (Area a,c) -> Maybe(Area a, Maybe(c))
                             >>= (returnRemnant unldrPos)          -- : (Area a,c) -> Maybe(Area a, Maybe(c))
                             >>= (return . fst)                    -- : (Area a, Maybe(c)) -> Maybe (Area a)


unload : (LoadF a c) -> (UnloadF a c) -> (Area a) -> LocationSignal -> Maybe(Area a)
unload ld unld area sig = let unldrPos = sig.from
                              ldrPos = sig.target

                              sig' = locationSignal ldrPos unldrPos
                           in 
                              load ld unld area sig'


loadProxy : (LoadF a c) -> (UnloadF a c) -> (Area a) -> LoadSignal -> Maybe(Area a)
loadProxy ld unld area sig = let sig' = asLocationSignal sig
                              in
                                case sig.action of
                                    Load    ->  load ld unld area sig'
                                    Unload  ->  unload ld unld area sig'


type Loader a = SF (LoadSignal) (Maybe(Area a))

loader : (LoadF a c) -> (UnloadF a c) -> (Area a) -> (Loader a)
loader ld unld area = pure (loadProxy ld unld area)