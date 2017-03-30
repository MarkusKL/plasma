module Physics
( Vector (V)
, GLfl4
, GLfloat
, Time
, Delta
, Velocity
, Position
, Acceleration
, move
, adjustLength
, absV
, (*-)
, v't
, (><)
) where

import Graphics.UI.GLUT (GLfloat)
import Util

data Vector = V GLfloat GLfloat GLfloat deriving (Show,Eq)
type GLfl4 = (GLfloat,GLfloat,GLfloat,GLfloat)
type Time = Int
type Delta = Time
type Velocity = Vector
type Position = Vector
type Acceleration = Vector

instance Num Vector where
  V a1 a2 a3 + V b1 b2 b3 = V (a1+b1) (a2+b2) (a3+b3)
  negate (V x y z) = V (negate x) (negate y) (negate z)
  V a1 a2 a3 * V b1 b2 b3 = V (a1*b1) (a2*b2) (a3*b3)
  abs (V x y z) = let len = sqrt (x^2 + y^2 + z^2) in V len len len
  signum v@(V x y z) = let a = absV v in V (x/a) (y/a) (z/a)
  fromInteger a = V (fromInteger a) 0 0
  
absV :: Vector -> GLfloat
absV v = let V a _ _ = abs v in a

move :: Delta -> Velocity -> Position -> Position
move d v p = fromIntegral d *- v + p

infixr 7 *- -- Scalar multiplication
(*-) :: GLfloat -> Vector -> Vector
a *- V a1 a2 a3 = V (a*a1) (a*a2) (a*a3)

adjustLength :: GLfloat -> Vector -> Vector
adjustLength l v3 = l *- signum v3

v't :: Vector -> (GLfloat,GLfloat,GLfloat)
v't (V x y z) = (x,y,z)

infix 8 >< -- Cross product
(><) :: Vector -> Vector -> Vector
(V u1 u2 u3) >< (V v1 v2 v3) = V (u2*v3 - u3*v2) (u3*v1 - u1*v3) (u1*v2 - u2*v1)
