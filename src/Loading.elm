module Loading where
import Automaton as Auto
import Area as A

data CargoState = Empty | Full
type Carrying a = A.Locatable { a | cargo:CargoState }

data LoadActions = Load | Unload
type LoadSignal a = A.LocationSignal (Carrying {a | loadAction:LoadActions})

load:(LoadSignal a)->Maybe(A.Area (Carrying a))
load sig = let ldr = sig.who
               target = sig.target

               area = ldr.A.Area
               ldee = area `A.get` target
               
               load' area ldr = A.add area ldr.location {ldr | cargo <- Full}
               unload' area ldee = A.add area ldee.location {ldee | cargo <- Empty}
            in
               case (ldee, ldr.cargo) of
                    (Just ldee', Empty) -> case ldee'.cargo of
                                                Full->Just ((area `unload'` ldee') `load'` ldr)
                                                Empty->Nothing
                    _ -> Nothing

unload:(LoadSignal a)->Maybe(A.Area (Carrying a))
unload sig = let ldee = sig.who
                 to = sig.target

                 area = ldr.A.Area
                 ldr = area `A.get` to
              in 
                 case ldr of
                    Just ldr' -> load ldr' ldee.location
                    Nothing -> Nothing

process:(LoadSignal a)->Maybe(A.Area (Carrying a))
process carrySig = case carrySig.loadAction of
                        Load -> load carrySig
                        Unload -> unload carrySig
                        _ -> Nothing


type Loader a = Auto.Automaton (LoadSignal a) Maybe(A.Area (Carrying a))

loader : Loader a
loader = Auto.pure (process)