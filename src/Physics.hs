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

g :: GLfloat -- Gravity
g = -0.0002

gt :: Acceleration -- Gravity triple
gt = (0,g,0)

move :: Delta -> Velocity -> Position -> Position
move d v p = fromIntegral d *- v +++ p

--pos :: (Body a) => Time -> a -> Position
--pos rt b = origin b +++ t *- velocity b +++ (t*t/2) *- gt
--    where t = fromIntegral $ rt - originTime b
