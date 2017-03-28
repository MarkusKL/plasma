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
, adjustLength
, len3
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

adjustLength :: GLfloat -> GLfl3 -> GLfl3
adjustLength l v3 = (l / len3 v3) *- v3

len3 :: GLfl3 -> GLfloat
len3 (x,y,z) = sqrt ( x^2 + y^2 + z^2 )
