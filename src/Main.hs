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

handleKeys :: Key -> KeyState -> Modifiers -> GL.Position -> WorldS ()
--handleKeys (Char 'n') Down _ _ w = addParticle (genParticle t) w
--  where t = wTime w
handleKeys _ _ _ _ = return ()

drawParticle :: Time -> Particle -> IO ()
drawParticle t p = do
    color $ c4 ~++~ (1,1,0,1) -- pColor p
    preservingMatrix (particle $ pPosition p)

particle :: Position -> IO ()
particle p = do
    t3 ~+~ v't p
    renderSphere

renderSphere :: IO ()
renderSphere = renderQuadric sphereStyle $ Sphere 1 3 3

sphereStyle :: QuadricStyle
sphereStyle = QuadricStyle Nothing NoTextureCoordinates Inside LineStyle
