module Moving where
import Automaton
import Area

type Position a = { a | occupied:Bool }

mv:(LocationSignal a)->Maybe(Area a)
mv sig = let
			toMv = sig.who
			newPos = sig.target

			whr = toMv.area
            from = toMv.location
            setPos toMv newPos = (toMv | location <- newPos)
          in
            case whr `get` newPos of
              	Just pos -> if pos.occupied
         					then Nothing
                    		else add (whr `remove` toMv) (toMv `setPos` newPos) newPos
                _ -> Nothing

type Motor a = Automaton (DirectionalSignal a) Maybe(Area a)

motor:Motor a
motor = pure (inDir mv)

type Moving a = { a | motor:(Motor a) }