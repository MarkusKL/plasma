module Main where

import Window
import World
import Util
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Graphics.UI.GLUT hiding (Position)
import qualified Graphics.UI.GLUT as GL (Position)

main :: IO ()
main = do
    initWindow newWorld drawWorld updateWorld handleKeys
    mainLoop

drawWorld :: World -> IO ()
drawWorld w = do
    preservingMatrix (mapM_ (drawParticle t) $ wParticles w)
    where t = wTime w

radius :: Num a => a
radius = 600

handleKeys :: Key -> KeyState -> Modifiers -> GL.Position -> World -> World
--handleKeys (Char 'n') Down _ _ w = addRockets (newRocket <$> (t+) <$> [1..100]) w
--    where t = wTime w
handleKeys (Char 'n') Down _ _ w = addParticle (Particle (randomPosition t) ((randomVelocity t) +++ randomTriple t (-0.01,0.01))) w
  where t = wTime w
handleKeys _ _ _ _ w = w

  -- On a circle with a radius of 200
randomPosition :: Time -> Position
randomPosition t = (cos v * 800, sin v * 800, 0)
  where (v:_) = randomRs (0,2*pi) (mkStdGen t)

randomVelocity :: Time -> Position
randomVelocity t = (sin v * (-0.1), cos v * 0.1, 0)
  where (v:_) = randomRs (0,2*pi) (mkStdGen t)

randomTriple :: (Random a) => Time -> (a,a) -> (a,a,a)
randomTriple d r = (r1,r2,r3)
  where (r1:r2:r3:_) = randomRs r (mkStdGen d)

drawParticle :: Time -> Particle -> IO ()
drawParticle t p = do
    color $ c4 ~++~ (1,1,0,1) -- pColor p
    preservingMatrix (particle $ pPosition p)

particle :: Position -> IO ()
particle p = do
    t3 ~+~ p
    renderSphere

renderSphere :: IO ()
renderSphere = renderQuadric sphereStyle $ Sphere 1 3 3

sphereStyle :: QuadricStyle
sphereStyle = QuadricStyle Nothing NoTextureCoordinates Inside LineStyle
