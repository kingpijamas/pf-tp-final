module AntColony.Main where

import AntColony.Utils.Matrix as M
import AntColony.Geography.Area as A
import AntColony.Geography.Direction as D

import AntColony.Capacities.Moving as Mv
import AntColony.Capacities.Loading as L
import AntColony.Capacities.Scenting as Sc
import AntColony.Capacities.Perceiving

import AntColony.Model.Terrain as T

import AntColony.Model.Moving
import AntColony.Model.Scenting
import AntColony.Model.Seeing
import AntColony.Model.Smelling
import AntColony.Model.Loading

main = plainText "Hello, World!"
