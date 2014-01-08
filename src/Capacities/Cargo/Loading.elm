module Capacities.Cargo.Loading where

import open Capacities.Cargo.Cargo
import open Geography.Area
import open Automaton
import open Utils.MaybeMonad

data LoadAction = Load | Unload

type Area' a b = Area (Carrying a b

type LoadSignal a b = { who:(Carrying a b)
                      , from:Coords
                      , target:Coords
                      , action:LoadAction
                      }

type LocationSignal a b = { who:(Carrying a b)
                           , from:Coords
                           , target:Coords
                           }

--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- TODO: 2 big ideas: One, functors (to solve the problem with implementation)
--                    The other, Area.add [(Coords,Stuff)] + [] Monad. It'd work wonders
--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


locationSignal : (Carrying a b) -> Coords -> Coords -> (LocationSignal a b)
locationSignal who from target = {who=who, from=from, target=target}

loadSignal : (Carrying a b) -> Coords -> Coords -> (LoadSignal a b)
loadSignal who from target = {who=who, from=from, target=target, action=Load}

unloadSignal : (Carrying a b) -> Coords -> Coords -> (LoadSignal a b)
unloadSignal who from target = {who=who, from=from, target=target, action=Unload}


load : (LoadF a b) -> (UnloadF a b) -> (Area' a b) -> (LocationSignal a b) -> Maybe(Area' a b)
load ld unld area sig = let ldr = sig.who
                            tPos = sig.target
                            getUnldr = area `get` tPos

                            load' (unldr', cargo) = case (ldr `ld` cargo) of
                                                        Just ldr' -> return (unldr', ldr')
                                                        _         -> Nothing

                            updateUnldr (unldr', ldr') = case(add area tPos unldr') of
                                                            Just area' -> return (area',ldr')
                                                            _          -> Nothing

                            updateLdr (area', ldr') = add area' (sig.from) ldr'
                         in
                            getUnldr                -- : Maybe (Carrying a b)
                            >>= (unld)              -- : (Carrying a b) -> Maybe(Carrying a b, a)
                            >>= (load')             -- : (Carrying a b,a) -> Maybe(Carrying a b, Carrying a b)
                            >>= (updateUnldr)       -- : (Carrying a b, Carrying a b) -> Maybe(Area' a b, Carrying a b)
                            >>= (updateLdr)         -- : (Area' a b, Carrying a b) -> Maybe(Area' a b)


unload : (LoadF a b) -> (UnloadF a b) -> (Area' a b) -> (LocationSignal a b) -> Maybe(Area' a b)
unload ld unld area sig = let unldr = sig.who
                              getLdr = area `get` sig.target

                              loadSignal' ldr = return (loadSignal ldr (sig.target) (sig.from))
                           in 
                              getLdr                    -- : Maybe(Carrying a b)
                              >>= (loadSignal')         -- : Carrying a b -> Maybe (LoadSignal a b)
                              >>= (load)                -- : LoadSignal a b -> Maybe(Area' a b)


loadProxy : (LoadF a b) -> (UnloadF a b) -> (Area' a b) -> (LoadSignal a b) -> Maybe(Area' a b)
loadProxy ld unld area sig = let sig' = locationSignal (sig.who) (sig.from) (sig.target)
                              in
                                case sig.action of
                                    Load    ->  load ld unld area sig'
                                    Unload  ->  unload ld unld area sig'


type Loader a b = Automaton (LoadSignal a b) (Maybe(Area' a b))

loader : (LoadF a b) -> (UnloadF a b) -> (Area' a b) -> (Loader a b)
loader ld unld area = pure (loadProxy ld unld area)