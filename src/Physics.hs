module Physics
( GLfl3
, GLfl4
, GLfloat
, Time
, Delta
, Velocity
, Position
, Acceleration
, move
, g
, gt
) where

import Graphics.UI.GLUT (GLfloat)
import Util

type GLfl3 = (GLfloat,GLfloat,GLfloat)
type GLfl4 = (GLfloat,GLfloat,GLfloat,GLfloat)
type Time = Int
type Delta = Time
type Velocity = GLfl3
type Position = GLfl3
type Acceleration = GLfl3

move :: Delta -> Velocity -> Position -> Position
move d v p = fromIntegral d *- v +++ p
