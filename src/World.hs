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
newWorld = World 0 (genParticle <$> [1..1000]) 

radius :: GLfloat
radius = 750

baseVel :: GLfloat
baseVel = 0.2

updateWorld :: Time -> World -> World
updateWorld t w = updateParticles d $ changeTime t w
  where d = t - wTime w

updateParticles :: Delta -> World -> World
updateParticles d w = w { wParticles = updateParticle d <$> wParticles w }

updateParticle :: Delta -> Particle -> Particle
updateParticle d p = circle d $ p { pPosition =  move d (pVelocity p) (pPosition p) }

circle :: Delta -> Particle -> Particle
circle d p = applyAcceleration d acc p
  where acc = adjustLength (-baseVel^2/radius) (pPosition p)

applyAcceleration :: Delta -> Acceleration -> Particle -> Particle
applyAcceleration d a p = p { pVelocity = pVelocity p +++ fromIntegral d *- a } 

addParticle :: Particle -> World -> World
addParticle p w = w { wParticles = p:wParticles w }

changeTime :: Time -> World -> World
changeTime t w = w { wTime = t } 

randomPosition :: Time -> Position
randomPosition t = (cos v * radius, sin v * radius, 0)
  where (v:_) = randomRs (0,2*pi) (mkStdGen t)

randomVelocity :: Time -> Position
randomVelocity t = (sin v * (-baseVel), cos v * baseVel, 0)
  where (v:_) = randomRs (0,2*pi) (mkStdGen t)

randomTriple :: (Random a) => Time -> (a,a) -> (a,a,a)
randomTriple d r = (r1,r2,r3)
  where (r1:r2:r3:_) = randomRs r (mkStdGen d)

genParticle :: Time -> Particle
genParticle t = (Particle (randomPosition t) ((randomVelocity t) +++ randomTriple (t+1) (-0.1*baseVel,0.1*baseVel)))

