import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless)
import Graphics.Rendering.OpenGL.Raw (glViewport)
import qualified Graphics.UI.GLFW as GLFW

mainLoop :: GLFW.Window -> IO ()
mainLoop window = do
    closeFlag <- GLFW.windowShouldClose window
    unless closeFlag $ do
      GLFW.pollEvents
      GLFW.swapBuffers window
      mainLoop window

main :: IO ()
main = do
    GLFW.init >>= \r -> unless r $ error "Failed to initialize GLFW"
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True -- needed for Mac OS X
    
    mw <- GLFW.createWindow 800 600 "Learn OpenGL" Nothing Nothing

    case mw of
      Nothing -> GLFW.terminate >> exitFailure
      Just window -> do
        GLFW.makeContextCurrent mw
        glViewport 0 0 800 600
        mainLoop window >> GLFW.terminate >> exitSuccess
