module Loading where
import Automaton
import Area

data CargoState = Empty | Full
type Carrying a = Locatable { a | cargo:CargoState }

data LoadActions = Load | Unload
type LoadSignal a = LocationSignal (Carrying {a | loadAction:LoadActions})

load:(LoadSignal a)->Maybe(Area (Carrying a))
load sig = let ldr = sig.who
               target = sig.target

               whr = ldr.area
               ldee = whr `get` target
               
               load' whr ldr = add whr ldr.location {ldr | cargo <- Full}
               unload' whr ldee = add whr ldee.location {ldee | cargo <- Empty}
            in
               case (ldee, ldr.cargo) of
                    (Just ldee', Empty) -> case ldee'.cargo of
                                                Full->Just ((whr `unload'` ldee') `load'` ldr)
                                                Empty->Nothing
                    _ -> Nothing

unload:(LoadSignal a)->Maybe(Area (Carrying a))
unload sig = let ldee = sig.who
                 to = sig.target

                 whr = ldr.area
                 ldr = whr `get` to
              in 
                 case ldr of
                    Just ldr' -> load ldr' ldee.location
                    Nothing -> Nothing

process:(LoadSignal a)->Maybe(Area (Carrying a))
process carrySig = case carrySig.loadAction of
                        Load -> load sig
                        Unload -> unload sig
                        _ -> Nothing


type Loader a = Automaton (LoadSignal a) Maybe(Area (Carrying a))

loader:LoadModule a
loader = pure (process)