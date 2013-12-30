module Loading where
import Automaton
import Area

data CargoState = Empty | Full
type Carrying a = Locatable { a | cargo:CargoState }

data LoadActions = Load | Unload
type CarrySignal a = LocationSignal (Carrying {a | loadAction:LoadActions})

load:(CarrySignal a)->Maybe(Area (Carrying a))
load sig = let ldr = sig.who
			   target = sig.target

			   whr = ldr.area
			   ldee = whr `get` target
			   
			   load' whr ldr = whr `add` {ldr | cargo <- Full}
			   unload' whr ldee = whr `add` {ldee | cargo <- Empty}
			in
			   case (ldee, toLoad.cargo) of
					(Just ldee', Empty) -> Just ((whr `unload'` ldee') `load'` ldr)
					_ -> Nothing

unload:(CarrySignal a)->Maybe(Area (Carrying a))
unload sig = let ldee = sig.who
				 to = sig.target

				 ldr = whr `get` to
			  in 
				 case ldr of
				 	Just ldr' -> load ldr' ldee.location
				 	Nothing -> Nothing

process:(CarrySignal a)->Maybe(Area (Carrying a))
process carrySig = case carrySig.loadAction of
						Load -> load sig
						Unload -> unload sig
						_ -> Nothing


type Loader a = Automaton (CarrySignal a) Maybe(Area (Carrying a))

loader:LoadModule a
loader = pure (process)