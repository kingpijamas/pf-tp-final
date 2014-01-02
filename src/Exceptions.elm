module Exceptions where

assert:Maybe a -> (a -> b) -> Maybe b
assert mbx f = case mbx of
                    Just sth -> Just (f sth)
                    Nothing -> Nothing


assert2:Maybe a -> Maybe b -> (a->b->c) -> Maybe c
assert2 mbx mby f = (assert mbx)

unify:


try:[Expression]->



tryCatch

--data ExtMaybe a b = Just a | Nothing | Throws Exception b

--type Exception a = { a | msg:String } 

----infixr 0 <!

----(<!):(a->b)->(Maybe a)->(Maybe b)
----(<!) f x = maybe Nothing f x

----(a->b->c)->(Maybe a)->(Maybe b)->(Maybe c)






------maybe : b -> (a -> b) -> Maybe a -> b



----(.):(b -> c) -> (a -> b) -> (a -> c)
----Function composition: (f . g == (\\x -> f (g x)))

--(<!):(a->b)-> Maybe a -> Maybe b
--f:(a->b->c)
--================================
--a=a
--b=b->c
--(<! f):Maybe a -> Maybe (b->c)


--(<!):(a->b)-> Maybe a -> Maybe b
--(<! f):Maybe a -> Maybe (b->c)


--(<! f):Maybe a -> Maybe (b->c)
--mbx:Maybe a
--===============================
--(<! f mbx):Maybe (b->c)

--1)
--  (XX):Maybe (b->c) -> Maybe b -> Maybe c
--  (XX) mbf mbb = case (mbf,mbb) of
--                  (Just f,Just b) -> Just (f b)
--                  _ -> Nothing

--  (XX) (<! f mbx) : Maybe b -> Maybe c

--  (XX) (<! f mbx) mby : Maybe c

--2)
--  (YY):Maybe (b->c) -> b -> Maybe c
--  (YY) mbf b = case mbf of
--                  Just f -> Just (f b)
--                  _ -> Nothing

--  (YY) (<! f mbx) : b -> Maybe c
    
--  (YY) (<! f mbx) y : Maybe c


--uncurry : (a -> b -> c) -> (a,b) -> c
--f:a->b->c
--=====================================
--(uncurry f):(a,b)->c

--(<!):(a->b)-> Maybe a -> Maybe b
--(uncurry f):(a,b)->c
--=====================================
--a=(a,b)
--b=c

--(<! (uncurry f)):Maybe (a,b) -> Maybe c


--(<!):(a->b)-> Maybe a -> Maybe b
--f:a->b->c->d
--===============================
--a=a->b->c
--b=d
--(<! f): Maybe (a->b->c) -> Maybe d

--1)
--  (<! f XX) : Maybe 


--  (XX):Maybe (b->c) -> Maybe b -> Maybe c



--=> Maybe b->Maybe c




--================================
--a=Maybe a
--b=Maybe (b->c)
--(<!(<! f)):Maybe (Maybe a) -> 





--(.):(b -> c) -> (a -> b) -> (a -> c)
--(<!):(a->b)->(Maybe a)->(Maybe b)
--====================================
--b=(a->b)
--c=(Maybe a)->(Maybe b)


--((<!).):(a'->(a->b))->(a'->(Maybe a)->(Maybe b))
--(<!):(a->b)->(Maybe a)->(Maybe b)
--====================================
--(((<!).)(<!)))


----pasar los args a tupla => 
----pasarlos a lista (? no se puede) => (quizas se podria pasar todos menos el 1ro y ver ..)
----ver con just si son todos null o no => 
----profit

