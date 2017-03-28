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
newWorld = World 0 [] 

radius :: GLflaoat
radius = 800

updateWorld :: Time -> World -> World
updateWorld t w = updateParticles d $ changeTime t w
  where d = t - wTime w

updateParticles :: Delta -> World -> World
updateParticles d w = w { wParticles = updateParticle d <$> wParticles w }

updateParticle :: Delta -> Particle -> Particle
updateParticle d p = circle d $ p { pPosition =  move d (pVelocity p) (pPosition p) }

circle :: Delta -> Particle -> Particle
circle d p = applyAcceleration d acc p
  where acc = adjustLength (-0.00001666) (pPosition p)

applyAcceleration :: Delta -> Acceleration -> Particle -> Particle
applyAcceleration d a p = p { pVelocity = pVelocity p +++ fromIntegral d *- a } 

addParticle :: Particle -> World -> World
addParticle p w = w { wParticles = p:wParticles w }

changeTime :: Time -> World -> World
changeTime t w = w { wTime = t } 
