module Capacities.Loading where

import Geography.Area as A
import Automaton as Auto
import Utils.MaybeMonad as M


(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

data CargoState = Empty | Full
type Carrying a = A.Locatable { a | cargo:CargoState }

data LoadActions = Load | Unload

type LocationSignal' a = A.LocationSignal (Carrying a)
type Area' a = A.Area (Carrying a)

type LoadSignal a = { who:A.Locatable (Carrying a)
                    , target:A.Coords
                    , loadAction:LoadActions
                    }

loadSignal : A.Locatable (Carrying a) -> A.Coords -> LoadSignal a
loadSignal who target = {who=who, target=target, loadAction=Load}

unloadSignal : A.Locatable (Carrying a) -> A.Coords -> LoadSignal a
unloadSignal who target = {who=who, target=target, loadAction=Unload}

load : Area' a -> LocationSignal' a -> Maybe(Area' a)
load area sig = let ldr = sig.who
                    target = sig.target
                 
                    assertLoads unldr = case (unldr.cargo, ldr.cargo) of
                                          (Empty,_) ->  Nothing
                                          (_,Full)  ->  Nothing
                                          _         ->  M.return unldr

                    unload' unldr = A.add area (unldr.location) {unldr | cargo <- Empty}

                    load' area = A.add area (ldr.location) {ldr | cargo <- Full}
                 in
                    (area `A.get` target)
                    >>= (assertLoads)
                    >>= (unload')
                    >>= (load')


unload : Area' a -> LocationSignal' a -> Maybe(Area' a)
unload area sig = let unldr = sig.who
                 
                      loadSignal' ldr = M.return (loadSignal ldr unldr.location)
                   in 
                      (area `A.get` sig.target) -- : Maybe(Carrying a)
                      >>= (loadSignal')        -- : Carrying a -> Maybe (LoadSignal a)
                      >>= (load)               -- : LoadSignal a -> Maybe(Area' a)


loadProxy : Area' a -> LoadSignal a -> Maybe(Area' a)
loadProxy area sig = let sig' = A.locationSignal (sig.who) (sig.target)
                      in
                         case sig.loadAction of
                            Load    ->  load area sig'
                            Unload  ->  unload area sig'


type Loader a = Auto.Automaton (LoadSignal a) (Maybe(Area' a))

loader : Area' a -> Loader a
loader area = Auto.pure (loadProxy area)