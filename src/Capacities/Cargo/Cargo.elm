module Capacities.Cargo.Cargo where

import Utils.MaybeMonad as M

type Carrying a b = { b | cargo:Maybe(a) }

type LoadF a b = (Carrying a b) -> a -> Maybe(Carrying a b)

load : LoadF a b
load ldr ld = case ldr.cargo of
                Nothing -> M.return ({ ldr | cargo <- Just ld})
                _ -> Nothing

type UnloadF a b = (Carrying a b) -> Maybe(Carrying a b, a)

unload : UnloadF a b
unload unldr = case unldr.cargo of
                Just cargo -> M.return ({ unldr | cargo <- Nothing }, cargo)
                _ -> Nothing