module Window
( initWindow
, mainLoop
, v3
, t3
, s3
, c3
, c4
) where

import Graphics.UI.GLUT
import Data.IORef

initWindow :: a
           -> (a -> IO ())
           -> (Int -> a -> a)
           -> (Key -> KeyState -> Modifiers -> Position -> a -> a)
           -> IO ()
initWindow start draw update keyCB = do
    getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer,DoubleBuffered,WithAlphaComponent]
    createWindow "Plasma"
    depthFunc $= Just Less
    lineSmooth $= Enabled
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lineWidth $= 1.5
    pointSize $= 1

    worldRef <- newIORef start

    keyboardMouseCallback $= Just (keyboardMouse keyCB worldRef)
    displayCallback $= (display draw update worldRef)
    idleCallback $= Just (idle)
    return ()

display :: (a -> IO ()) -> (Int -> a -> a) -> IORef a -> DisplayCallback
display drawFunc updateFunc worldRef = do
    
    clear [ColorBuffer,DepthBuffer]

    loadIdentity
    perspective 90 1 0.01 2.01
    t3 0 0 (-1.01)
    s3 0.001 0.001 0.001

    time <- get elapsedTime
    print time
    worldRef $~! (updateFunc (time*10))
    world <- get worldRef
    drawFunc world

    swapBuffers

keyboardMouse :: (Key -> KeyState -> Modifiers -> Position -> a -> a)
              -> IORef a
              -> KeyboardMouseCallback
keyboardMouse _ _ (Char 'f') Down _ _ = fullScreenToggle
keyboardMouse _ _ (Char '\ESC') Down _ _ = exit
keyboardMouse f worldRef key keySt mod pos = worldRef $~! f key keySt mod pos


v3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
v3 x y z = vertex $ Vertex3 x y z

t3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
t3 x y z = translate $ Vector3 x y z

s3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
s3 x y z = scale x y z

c3 :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat
c3 r g b = Color4 r g b 1

c4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat
c4 = Color4

idle :: IdleCallback
idle = postRedisplay Nothing
