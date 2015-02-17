import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL.Raw (glViewport, glClear, glClearColor, gl_COLOR_BUFFER_BIT)
import qualified Graphics.UI.GLFW as GLFW

mainLoop :: GLFW.Window -> IO ()
mainLoop window = do
    closeFlag <- GLFW.windowShouldClose window
    unless closeFlag $ do
      GLFW.pollEvents

      glClearColor 0.2 0.3 0.3 1.0
      glClear gl_COLOR_BUFFER_BIT

      GLFW.swapBuffers window
      mainLoop window

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ =
    when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose window True

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
        GLFW.setKeyCallback window (Just keyCallback)
        glViewport 0 0 800 600
        mainLoop window >> GLFW.terminate >> exitSuccess
