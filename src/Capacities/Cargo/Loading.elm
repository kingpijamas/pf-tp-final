module Capacities.Cargo.Loading where

import Capacities.Cargo.Cargo as C
import Geography.Area as A
import Automaton as Auto
import Utils.MaybeMonad as M

(>>=) = (>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

data LoadAction = Load | Unload

type Area' a b = A.Area (C.Carrying a b)

type LoadSignal a b = { who:(C.Carrying a b)
                      , from:A.Coords
                      , target:A.Coords
                      , action:LoadAction
                      }

type LocationSignal a b = { who:(C.Carrying a b)
                           , from:A.Coords
                           , target:A.Coords
                           }

locationSignal : (C.Carrying a b) -> A.Coords -> A.Coords -> (LocationSignal a b)
locationSignal who from target = {who=who, from=from, target=target}

loadSignal : (C.Carrying a b) -> A.Coords -> A.Coords -> (LoadSignal a b)
loadSignal who from target = {who=who, from=from, target=target, action=Load}

unloadSignal : (C.Carrying a b) -> A.Coords -> A.Coords -> (LoadSignal a b)
unloadSignal who from target = {who=who, from=from, target=target, action=Unload}


load : (C.LoadF a b) -> (C.UnloadF a b) -> (Area' a b) -> (LocationSignal a b) -> Maybe(Area' a b)
load ld unld area sig = let ldr = sig.who
                            tPos = sig.target
                            getUnldr = area `A.get` tPos

                            load' (unldr', cargo) = case (ldr `ld` cargo) of
                                                        Just ldr' -> M.return (unldr', ldr')
                                                        _         -> Nothing

                            updateUnldr (unldr', ldr') = (A.add area tPos unldr', ldr')

                            updateLdr (area', ldr') = A.add area' (sig.from) ldr'
                         in
                            getUnldr                -- : Maybe (Carrying a b)
                            >>= (unld)              -- : (Carrying a b) -> Maybe(Carrying a b, a)
                            >>= (load')             -- : (Carrying a b,a) -> Maybe(Carrying a b, Carrying a b)
                            >>= (updateUnldr)       -- : (Carrying a b, Carrying a b) -> Maybe(Area' a b, Carrying a b)
                            >>= (updateLdr)         -- : (Area' a b, Carrying a b) -> Maybe(Area' a b)


unload : (C.LoadF a b) -> (C.UnloadF a b) -> (Area' a b) -> (LocationSignal a b) -> Maybe(Area' a b)
unload ld unld area sig = let unldr = sig.who
                              getLdr = area `A.get` sig.target

                              loadSignal' ldr = M.return (loadSignal ldr (sig.target) (sig.from))
                           in 
                              getLdr                    -- : Maybe(Carrying a b)
                              >>= (loadSignal')         -- : Carrying a b -> Maybe (LoadSignal a b)
                              >>= (load)                -- : LoadSignal a b -> Maybe(Area' a b)


loadProxy : (C.LoadF a b) -> (C.UnloadF a b) -> (Area' a b) -> (LoadSignal a b) -> Maybe(Area' a b)
loadProxy ld unld area sig = let sig' = locationSignal (sig.who) (sig.from) (sig.target)
                              in
                                case sig.action of
                                    Load    ->  load ld unld area sig'
                                    Unload  ->  unload ld unld area sig'


type Loader a b = Auto.Automaton (LoadSignal a b) (Maybe(Area' a b))

loader : (C.LoadF a b) -> (C.UnloadF a b) -> (Area' a b) -> (Loader a b)
loader ld unld area = Auto.pure (loadProxy ld unld area)