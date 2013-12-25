module Movement where
import Automaton
import Area
import CardinalDirs

type Motor a = Automaton (Locatable a,CardinalDirs) Maybe(Area)

inDir:((Locatable a)->Coords->Maybe(Area a))->(Locatable a,CardinalDirs)->Maybe(Area a)
inDir f (loc,dir) = let proposed = from+dir
                    in 
                      case proposed of
                        Nothing -> Nothing
                        Just to -> f a to

mv:(Locatable a)->Coords->Maybe(Area a)
mv toMv newPos = let whr = toMv.area
                     from = toMv.location
                 in
                   case whr `get` newPos
                       Just sth -> Nothing
                       Nothing -> add (whr `remove` toMv) (toMv | location <- newPos) newPos 

motor:Motor a
motor = pure.(inDir mv)

type Moveable a = { a | motor:(Motor a) }

--Draft para hacer esto:
---un motor es un automata. La funcion con la que se crea el motor es la estrategia/el tipo de movimiento-
--{--Ahora bien, viendolo mejor, por no pensar el motor, solo actuar de forma reactiva, 
----seria SOLO el TIPO de movimiento la funcion, y NO la estrategia--}