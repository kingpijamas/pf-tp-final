module Moving where
import Automaton
import Area

type Position a = { a | occupied:Bool }

{-- FIXME:
	the area should either be a M.Matrix (Maybe a) or another type, because
	'Nothing's symbolise both errors and empty spots in the Matrix
--}

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

--Draft para hacer esto:
---un motor es un automata. La funcion con la que se crea el motor es la estrategia/el tipo de movimiento-
--{--Ahora bien, viendolo mejor, por no pensar el motor, solo actuar de forma reactiva, 
----seria SOLO el TIPO de movimiento la funcion, y NO la estrategia--}