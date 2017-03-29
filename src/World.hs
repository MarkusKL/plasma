module World 
( Time
, World
, wTime
, wParticles
, Particle (Particle)
, pPosition
, pVelocity
, newWorld
, updateWorld
, Position
, addParticle
, radius
, genParticle
, baseVel
, v't
) where

import System.Random
import Util
import Physics

data World = World { wTime :: Time
                   , wParticles :: [Particle]
                   }

data Particle = Particle { pPosition :: Position
                         , pVelocity :: Velocity
                         }

newWorld :: World
newWorld = World 0 genParticle

radius :: GLfloat
radius = 750

baseVel :: GLfloat
baseVel = 0.2

updateWorld :: Time -> World -> World
updateWorld t w = updateParticles d $ changeTime t w
  where d = t - wTime w

updateParticles :: Delta -> World -> World
updateParticles d w = w { wParticles = pinchAll (updateParticle d <$> wParticles w) }

updateParticle :: Delta -> Particle -> Particle
updateParticle d p = circle d $ p { pPosition =  move d (pVelocity p) (pPosition p) }

circle :: Delta -> Particle -> Particle
circle d p = applyAcceleration d acc p
  where acc = adjustLength (-baseVel^2/radius) (pPosition p)

applyAcceleration :: Delta -> Acceleration -> Particle -> Particle
applyAcceleration d a p = p { pVelocity = pVelocity p + fromIntegral d *- a } 

pinchAll :: [Particle] -> [Particle]
pinchAll ps = fmap (pinch ps) ps

pinch :: [Particle] -> Particle -> Particle
pinch ps p = p { pVelocity = pVelocity p + foldr (\p' v -> pinchTwo p' p + v) 0 ps }

pinchTwo :: Particle -> Particle -> Velocity
pinchTwo p p' = if pPosition p == pPosition p'
  then 0
  else (0.03 / absV pos)^2 *- pos
  where pos = pPosition p + (-1) *- pPosition p'

addParticle :: Particle -> World -> World
addParticle p w = w { wParticles = p:wParticles w }

changeTime :: Time -> World -> World
changeTime t w = w { wTime = t } 

randomPosition :: Time -> Position
randomPosition t = V (cos v * radius) (sin v * radius) 0
  where (v:_) = randomRs (0,2*pi) (mkStdGen t)

randomVelocity :: Time -> Position
randomVelocity t = V (sin v * (-baseVel)) (cos v * baseVel) 0
  where (v:_) = randomRs (0,2*pi) (mkStdGen t)

randomTriple :: Time -> (GLfloat,GLfloat) -> Vector
randomTriple d r = V r1 r2 r3
  where (r1:r2:r3:_) = randomRs r (mkStdGen d)

genParticle :: [Particle]
genParticle = map (\(r1,r2,r3,r4) -> (Particle (randomPosition r1 + randomTriple (r4+1) (-100*baseVel,100*baseVel)) ((randomVelocity r1) + randomTriple (r3+1) (-0.1*baseVel,0.1*baseVel)))) $ take 500 rs
  where rs = splitList . randoms $ mkStdGen 100

splitList :: [a] -> [(a,a,a,a)]
splitList (x1:x2:x3:x4:xs) = (x1,x2,x3,x4):splitList xs

