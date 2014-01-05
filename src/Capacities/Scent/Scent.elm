module Capacities.Scent.Scent where

type Scent = Int
type Scentable a = { a | scent:Scent }

type ScentF a = ( Scentable a -> Scentable a )

scent : ScentF a
scent target = { target | scent <- (target.scent)+1 }

unscent : ScentF a
unscent target = { target | scent <- (target.scent)-1 }