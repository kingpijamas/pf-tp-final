module Capacities.Cargo.Cargo where

import open Utils.MaybeMonad

type Carrying a b = { b | cargo:Maybe(a) }

type LoadF a b = (Carrying a b) -> a -> Maybe(Carrying a b)

load : LoadF a b
load ldr ld = case ldr.cargo of
                Nothing -> return ({ ldr | cargo <- Just ld})
                _ -> Nothing

type UnloadF a b = (Carrying a b) -> Maybe(Carrying a b, a)

unload : UnloadF a b
unload unldr = case unldr.cargo of
                Just cargo -> return ({ unldr | cargo <- Nothing }, cargo)
                _ -> Nothing