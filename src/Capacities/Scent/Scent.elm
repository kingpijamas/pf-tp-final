module Capacities.Scent.Scent where

type Scent = Int
type Scentable a = { a | scent:Scent }

scent target = { target | scent <- (target.scent)+1 }
unscent target = { target | scent <- (target.scent)-1 }