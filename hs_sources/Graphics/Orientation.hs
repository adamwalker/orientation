module Graphics.Orientation where

import Control.Monad
import Foreign.Storable (sizeOf)
import Control.Monad.Trans.Either

import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Linear as L

import Pipes

import Paths_orientation

drawOrientation :: [GLfloat] -> [GLfloat] -> IO (Either String (Consumer (Quaternion GLfloat) IO ()))
drawOrientation verts norms = runEitherT $ do
    lift $ setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg

    res <- lift $ G.init
    unless res (left "error initializing glfw")

    res' <- lift $ createWindow 1024 768 "" Nothing Nothing
    win <- maybe (left "error creating window") return res'

    lift $ do
        makeContextCurrent (Just win)

        vertFN <- getDataFileName "shaders/shader.vert"
        fragFN <- getDataFileName "shaders/shader.frag"

        --Load the shaders
        vs <- loadShader VertexShader   vertFN
        fs <- loadShader FragmentShader fragFN
        p  <- linkShaderProgram [vs, fs]

        --Set stuff
        depthFunc $= Just Less
        clearColor $= Color4 1 1 1 1
        currentProgram $= Just p

        --Coord per vertex variable
        let vbd :: [GLfloat]
            vbd = verts 
        ab <- makeBuffer ArrayBuffer vbd

        loc <- get $ attribLocation p "coord"

        let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 3
            vad    = VertexArrayDescriptor 3 Float stride offset0

        bindBuffer ArrayBuffer  $= Just ab
        vertexAttribArray   loc $= Enabled
        vertexAttribPointer loc $= (ToFloat, vad)
    
        --Normal per vertex variable
        let nbd :: [GLfloat]
            nbd = norms
        nb <- makeBuffer ArrayBuffer nbd

        loc <- get $ attribLocation p "normal"

        let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 3
            vad    = VertexArrayDescriptor 3 Float stride offset0

        bindBuffer ArrayBuffer  $= Just nb
        vertexAttribArray   loc $= Enabled
        vertexAttribPointer loc $= (ToFloat, vad)

        --The model, view and projection matrices
        let pm  = projectionMatrix (deg2rad 30 :: GLfloat) (1024/768) 0.1 10
            c   = camMatrix $ dolly (V3 0 0 6) fpsCamera

        --Get our uniform locations
        mvpLoc <- get $ uniformLocation p "mvp"
        mvLoc  <- get $ uniformLocation p "mv"
        vLoc   <- get $ uniformLocation p "v"

        return $ for cat $ \res -> do
            let rotation = mkTransformation res (V3 0 0 0)
            let mvp = pm !*! c !*! rotation
            let mv  = c !*! rotation
            lift $ do
                asUniform mvp mvpLoc
                asUniform mv  mvLoc
                asUniform c   vLoc
                clear [ColorBuffer, DepthBuffer]
                drawArrays Triangles 0 (fromIntegral $ length vbd `quot` 3)
                swapBuffers win

