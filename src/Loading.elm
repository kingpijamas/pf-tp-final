module Loading where
import Area as A
import Automaton as Auto
import MaybeMonad as M

(>>=)=(>>=) {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

data CargoState = Empty | Full
type Carrying a = A.Locatable { a | cargo:CargoState }

data LoadActions = Load | Unload

type LoadSignal a = { who:A.Locatable (Carrying a)
                    , target:A.Coords
                    , loadAction:LoadActions
                    }

type Area' a = A.Area (Carrying a)

loadSignal : A.Locatable (Carrying a) -> A.Coords -> LoadSignal a
loadSignal who target = {who=who, target=target, loadAction=Load}

unloadSignal : A.Locatable (Carrying a) -> A.Coords -> LoadSignal a
unloadSignal who target = {who=who, target=target, loadAction=Unload}

load : LoadSignal a -> Maybe(Area' a)
load sig = let ldr = sig.who
               target = sig.target
               area = ldr.area

               assertLoads unldr = case (unldr.cargo, ldr.cargo) of
                                     (Empty,_) -> Nothing
                                     (_,Full) -> Nothing
                                     _ -> M.return unldr

               unload' unldr = A.add area unldr.location {unldr | cargo <- Empty}

               load' area = A.add area ldr.location {ldr | cargo <- Full}

            in
               (area `A.get` target)
                >>= (assertLoads)
                >>= (unload')
                >>= (load')


unload : LoadSignal a -> Maybe(Area' a)
unload sig = let unldr = sig.who
                 area = unldr.area

                 loadSignal' ldr = M.return (loadSignal ldr unldr.location)
              in 
                 (area `A.get` sig.target) -- : Maybe(Carrying a)
                  >>= (loadSignal')        -- : Carrying a -> Maybe (LoadSignal a)
                  >>= (load)               -- : LoadSignal a -> Maybe(Area' a)


loadProxy : LoadSignal a -> Maybe(Area' a)
loadProxy sig = case sig.loadAction of
                  Load -> load sig
                  Unload -> unload sig


type LoadModule a = Auto.Automaton (LoadSignal a) Maybe(Area' a)

loadModule : LoadModule a
loadModule = Auto.pure(loadProxy)












--load:(LoadSignal a)->Maybe(A.Area (Carrying a))
--load sig = let ldr = sig.who
--               target = sig.target

--               area = ldr.A.Area
--               ldee = area `A.get` target
               
--               load' area ldr = A.add area ldr.location {ldr | cargo <- Full}
--               unload' area ldee = A.add area ldee.location {ldee | cargo <- Empty}
--            in



--               case (ldee, ldr.cargo) of
--                    (Just ldee', Empty) -> case ldee'.cargo of
--                                                Full->Just ((area `unload'` ldee') `load'` ldr)
--                                                Empty->Nothing
--                    _ -> Nothing



--1)
--    ldFrom = area `A.get` target -- : Maybe (Carrying a)
--2)
--    \ldFrom -> case (ldFrom.cargo, ldTo.cargo) of
--                    (Empty,_) -> Nothing
--                    (_,Full) -> Nothing
--                    _ -> return ldFrom

--    -- : Carrying a -> Maybe (Carrying a)

--    assertLoads ldFrom = case (ldFrom.cargo, ldTo.cargo) of
--                            (Empty,_) -> Nothing
--                            (_,Full) -> Nothing
--                            (_,_) -> return ldFrom
--3)
--    \ldFrom -> area `unload` ldFrom

--    -- : Carrying a -> Maybe (Area (Carrying a))

--    unload' ldFrom = A.add area ldFrom {ldee | cargo <- Empty}

--4)
--    \area -> area `load` ldr

--    -- : Area (Carrying a) -> Maybe (Area (Carrying a))

--    load' area = A.add area ldTo {ldr | cargo <- Full}
















































