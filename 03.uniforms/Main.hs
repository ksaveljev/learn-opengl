import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless, when)
import Foreign.Storable (sizeOf)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw (glViewport, glClear, glClearColor, gl_COLOR_BUFFER_BIT)
import qualified Graphics.UI.GLFW as GLFW

vertices :: [GLfloat]
vertices = [-0.5, -0.5, 0.0,
             0.5, -0.5, 0.0,
             0.0,  0.5, 0.0]

mainLoop :: GLFW.Window -> ShaderProgram -> VAO -> IO ()
mainLoop window shaderProgram vao = do
    closeFlag <- GLFW.windowShouldClose window
    unless closeFlag $ do
      GLFW.pollEvents

      glClearColor 0.2 0.3 0.3 1.0
      glClear gl_COLOR_BUFFER_BIT

      currentProgram $= Just (program shaderProgram)

      Just timeValue <- GLFW.getTime
      let greenValue = (sin timeValue / 2) + 0.5;
      let vertexColorLocation = getUniform shaderProgram "outColor"
      uniformVec vertexColorLocation $= map realToFrac [0.0, greenValue, 0.0, 1.0]

      withVAO vao $ drawArrays Triangles 0 3

      GLFW.swapBuffers window
      mainLoop window shaderProgram vao

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

        vbo <- makeBuffer ArrayBuffer vertices
        shaderProgram <- simpleShaderProgram "03.uniforms/simple.vs" "03.uniforms/simple.fs"

        vao <- makeVAO $ do
          let posn = getAttrib shaderProgram "position"
              stride = fromIntegral $ sizeOf (undefined::GLfloat) * 3
              vad = VertexArrayDescriptor 3 Float stride offset0
          bindBuffer ArrayBuffer $= Just vbo
          vertexAttribPointer posn $= (ToFloat, vad)
          vertexAttribArray posn $= Enabled
          bindBuffer ArrayBuffer $= Nothing

        mainLoop window shaderProgram vao >> GLFW.terminate >> exitSuccess
