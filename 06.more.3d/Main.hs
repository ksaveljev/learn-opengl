{-# LANGUAGE ScopedTypeVariables #-}

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless, when)
import Foreign.Storable (sizeOf)
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D (projectionMatrix)
import qualified Linear as L
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW

vertices :: [GLfloat]
vertices = [ -0.5, -0.5, -0.5,  0.0, 0.0,
              0.5, -0.5, -0.5,  1.0, 0.0,
              0.5,  0.5, -0.5,  1.0, 1.0,
              0.5,  0.5, -0.5,  1.0, 1.0,
             -0.5,  0.5, -0.5,  0.0, 1.0,
             -0.5, -0.5, -0.5,  0.0, 0.0,

             -0.5, -0.5,  0.5,  0.0, 0.0,
              0.5, -0.5,  0.5,  1.0, 0.0,
              0.5,  0.5,  0.5,  1.0, 1.0,
              0.5,  0.5,  0.5,  1.0, 1.0,
             -0.5,  0.5,  0.5,  0.0, 1.0,
             -0.5, -0.5,  0.5,  0.0, 0.0,

             -0.5,  0.5,  0.5,  1.0, 0.0,
             -0.5,  0.5, -0.5,  1.0, 1.0,
             -0.5, -0.5, -0.5,  0.0, 1.0,
             -0.5, -0.5, -0.5,  0.0, 1.0,
             -0.5, -0.5,  0.5,  0.0, 0.0,
             -0.5,  0.5,  0.5,  1.0, 0.0,

              0.5,  0.5,  0.5,  1.0, 0.0,
              0.5,  0.5, -0.5,  1.0, 1.0,
              0.5, -0.5, -0.5,  0.0, 1.0,
              0.5, -0.5, -0.5,  0.0, 1.0,
              0.5, -0.5,  0.5,  0.0, 0.0,
              0.5,  0.5,  0.5,  1.0, 0.0,

             -0.5, -0.5, -0.5,  0.0, 1.0,
              0.5, -0.5, -0.5,  1.0, 1.0,
              0.5, -0.5,  0.5,  1.0, 0.0,
              0.5, -0.5,  0.5,  1.0, 0.0,
             -0.5, -0.5,  0.5,  0.0, 0.0,
             -0.5, -0.5, -0.5,  0.0, 1.0,

             -0.5,  0.5, -0.5,  0.0, 1.0,
              0.5,  0.5, -0.5,  1.0, 1.0,
              0.5,  0.5,  0.5,  1.0, 0.0,
              0.5,  0.5,  0.5,  1.0, 0.0,
             -0.5,  0.5,  0.5,  0.0, 0.0,
             -0.5,  0.5, -0.5,  0.0, 1.0
           ]

indices :: [GLuint]
indices = [ 0, 1, 3, -- first triangle
            1, 2, 3] -- second triangle

mainLoop :: GLFW.Window -> ShaderProgram -> VAO -> TextureObject -> TextureObject -> IO ()
mainLoop window shaderProgram vao containerTexture awesomefaceTexture = do
    closeFlag <- GLFW.windowShouldClose window
    unless closeFlag $ do
      GLFW.pollEvents

      clearColor $= Color4 0.2 0.3 0.3 1.0
      clear [ColorBuffer, DepthBuffer]

      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just containerTexture
      setUniform shaderProgram "ourTexture1" (Index1 (0::GLint))
      activeTexture $= TextureUnit 1
      textureBinding Texture2D $= Just awesomefaceTexture
      setUniform shaderProgram "ourTexture2" (Index1 (1::GLint))

      currentProgram $= Just (program shaderProgram)

      Just glfwTime <- GLFW.getTime

      let model :: L.M44 GLfloat = L.m33_to_m44 . L.fromQuaternion $ L.axisAngle (L.V3 0.5 1.0 0.0) (50.0 * realToFrac glfwTime * pi / 180)
          view :: L.M44 GLfloat = L.mkTransformationMat L.eye3 $ L.V3 (0.0::GLfloat) 0.0 (-3.0)
          projection :: L.M44 GLfloat = projectionMatrix (45.0 * pi / 180) (800.0 / 600.0) 0.1 100.0

      asUniform model $ getUniform shaderProgram "model"
      asUniform view $ getUniform shaderProgram "view"
      asUniform projection $ getUniform shaderProgram "projection"

      withVAO vao $ drawArrays Triangles 0 36

      GLFW.swapBuffers window
      mainLoop window shaderProgram vao containerTexture awesomefaceTexture

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
        viewport $= (Position 0 0, Size 800 600)
        depthFunc $= Just Less

        vbo <- makeBuffer ArrayBuffer vertices
        ebo <- makeBuffer ElementArrayBuffer indices
        shaderProgram <- simpleShaderProgram "06.more.3d/simple.vs" "06.more.3d/simple.fs"

        vao <- makeVAO $ do
          let posn = getAttrib shaderProgram "position"
              texCoordn = getAttrib shaderProgram "texCoord"
              stride = fromIntegral $ sizeOf (undefined::GLfloat) * 5
              vadPosition = VertexArrayDescriptor 3 Float stride offset0
              vadTexCoord = VertexArrayDescriptor 2 Float stride (offsetPtr $ 3 * sizeOf (undefined::GLfloat))
          bindBuffer ArrayBuffer $= Just vbo
          bindBuffer ElementArrayBuffer $= Just ebo
          vertexAttribPointer posn $= (ToFloat, vadPosition)
          vertexAttribArray posn $= Enabled
          vertexAttribPointer texCoordn $= (ToFloat, vadTexCoord)
          vertexAttribArray texCoordn $= Enabled
          bindBuffer ArrayBuffer $= Nothing

        -- Texture 1
        Right containerTexture <- readTexture "06.more.3d/container.jpg"
        textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)
        generateMipmap Texture2D $= Enabled
        textureBinding Texture2D $= Nothing

        -- Texture 2
        Right awesomefaceTexture <- readTexture "06.more.3d/awesomeface.png"
        textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)
        generateMipmap Texture2D $= Enabled
        textureBinding Texture2D $= Nothing

        mainLoop window shaderProgram vao containerTexture awesomefaceTexture >> GLFW.terminate >> exitSuccess
