module Loading where
import Automaton
import Area

type Scent = Int
type Scentable a = Locatable { a | scent:Scent }

type DirectionSignal a = LocationSignal a

scent:(ScentSignal a)->Maybe(Area (Scentable a))
scent sig = let whr = (sig.who).area
                target = sig.target

                scntee = whr `get` target
             in
                case scntee of
                    Just scntee' -> whr `update` { scntee' | scent <- (scntee'.scent)+1 }
                    _ -> Nothing

type Scenter a = Automaton (ScentSignal a) Maybe(Area (Scentable a))

loader:LoadModule a
loader = pure (process)