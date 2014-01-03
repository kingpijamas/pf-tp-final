module Moving where
import Area as A
import Automaton as Auto
import MaybeMonad as M
import AutomatonExt as Ext

(>>=)=(>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}
(>>>)=(>>>) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

type Position a = { a | occupied:Bool }

type DirectionalSignal' a = A.DirectionalSignal (Position a)
type LocationSignal' a = A.LocationSignal (Position a)
type Area' a = A.Area (Position a)  

mv : LocationSignal' a -> Maybe (Area' a)
mv sig = let 
             toMv = sig.who
             area = toMv.area
             from = toMv.location
             to = sig.target

             getTargetPos = area `A.get` to

             getOccupiation targetPos = M.return (targetPos.occupied)
                
             clearIfNotOcc occ = if not occ
                                   then M.return (area `A.remove` from) {-- FIXME sth smells weird here --}
                                   else Nothing

             setPos toMv to = { toMv | location <- to }

             mv' area = A.add area to (toMv `setPos` to)
          in
             getTargetPos
              >>= (getOccupiation)
              >>= (clearIfNotOcc) {--TODO Maybe not the nicest name --}
              >>= (mv')

type Motor a = Auto.Automaton (DirectionalSignal' a) (Maybe(Area' a))

motor : Motor a
motor = Auto.pure(A.toLocSig) >>> Ext.impure(mv)

type Moving a = { a | motor:(Motor a) } {--TODO--}


--Proof for the replacements underway in Moving.elm

 {--OLD
          let
            toMv = sig.who
            area = toMv.area
            from = toMv.location
            to = sig.target

            --setPos:A.Locatable (Position a) -> A.Coords -> A.Locatable (Position a)
            setPos toMv to = { toMv | location <- to }
            
            --remove:A.Area a -> A.Coords -> Maybe (A.Area a)
            remove area from  = area `A.remove` from

            --
            add area to toMv = case (area `remove` from) of
                                  Just area' -> A.add area' to (toMv `setPos` to)
                                 _ -> Nothing

           isOccupied pos = pos.occupied 

         in

           case area `A.get` to of
               Just pos -> if pos.occupied
                           then add area to toMv
                           else Nothing
               _ -> Nothing
--}

{--NEW--}
--pure : (a -> b) -> Automaton a b

--state : b -> (a -> b -> b) -> Automaton a b

--run : Automaton a b -> b -> Signal a -> Signal b


--Automaton (Maybe a) (Maybe b)

--pure : (Maybe a -> Maybe b) -> Automaton (Maybe a) (Maybe b)

--(>>>) : Automaton a b -> Automaton b c -> Automaton a c



--Maybe b -> Signal (Maybe a) -> Signal (Maybe b)


--1)Coords->Maybe(Position)
--a1 = pure (A.get) --: Automaton Coords Maybe(Position) 

--2)Maybe(Position)->Maybe(Bool)
--a2 = pure (\\mbpos -> mbpos >>= (\\pos -> return (pos.occupied))) --: Automaton Maybe(Position) Maybe(Bool)

--getOccupation pos = return (pos.occupied)

--a2 = pure (\\mbpos -> mbpos >>= getOccupation)


--3)Maybe(Bool)->Maybe(Area)
--a3 = pure (\\mbocc -> mbocc >>= (\\occ -> if not occ
--                      then return (area `remove` from)
--                      else Nothing)) -- : Automaton Maybe(Bool) -> Maybe(Area)

--clearIfTheresRoom occ = if not occ
--              then return (area `remove` from)
--              else Nothing

--a3 = pure (\\mbocc -> mbocc >>= clearIfTheresRoom)

--4)Maybe(Area)->Maybe(Area)
--a4 = pure (\\mbarea -> mbarea >>= (\\area -> add area to (toMv `setPos` to)))

--add' area = add area to (toMv `setPos` to))

--a4 = pure (\\mbarea -> mbarea >>= add')

--5) profit

--================================================

-- pure (A.get) 
-->>>
-- pure (\\mbpos -> mbpos >>= getOccupation)
-->>>
-- pure (\\mbocc -> mbocc >>= clearIfTheresRoom)
-->>>
-- pure (\\mbarea -> mbarea >>= (\\area -> add area to (toMv `setPos` to)))
-->>>
-- pure (\\mbarea -> mbarea >>= add')