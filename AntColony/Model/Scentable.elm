module AntColony.Model.Scentable where

type Pheromone = Int

type Scentable a = { a | scent : Maybe(Pheromone) }

scentIncrement = 20
scentDecrement = 1

scent : Scentable a -> Scentable a
scent scentable = let scent' = case scentable.scent of
                                    Nothing -> scentIncrement
                                    Just sc -> sc + scentIncrement
                   in 
                      { scentable | scent <- Just scent' }

unscent : Scentable a -> Maybe(Scentable a)
unscent scentable = let zeroAsNothing x = if x == 0
                                          then Nothing
                                          else Just x
                     in
                        case scentable.scent of
                             Nothing -> Nothing
                             Just sc -> Just { scentable | scent <- zeroAsNothing (sc-scentDecrement) }

getPheromone : Scentable a -> Pheromone
getPheromone scentable = case scentable.scent of
                              Just ph -> ph
                              _ -> 0
