module Graphics.Orientation (
    parseObj,
    setupGLFW,
    drawOrientation
    ) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List.Split
import Foreign.Storable (sizeOf)
import Control.Monad.Trans.Either

import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Linear as L

import Pipes

import Paths_orientation

parseObj :: [String] -> ([GLfloat], [GLfloat])
parseObj lines = (concat verts, concat norms)
    where
    (verts, norms) = unzip faces
    vertices  = toMap $ map parse3Coord $ map (drop 2) $ filter (command "v")  lines
    texCoords = toMap $ map parse2Coord $ map (drop 3) $ filter (command "vt") lines
    normals   = toMap $ map parse3Coord $ map (drop 3) $ filter (command "vn") lines
    toMap     = Map.fromList . zip [1..]
    parse3Coord str = [read x :: GLfloat, read y, read z]
        where
        [x, y, z] = splitOn " " str
    parse2Coord str = [read x :: GLfloat, read y]
        where
        [x, y] = splitOn " " str
    faces     = map doFaces $ map (drop 2) $ filter (command "f")  lines
    command c line = take (length c + 1) line == (c ++ " ")
    doFaces line = (vv1 ++ vv2 ++ vv3, n1 ++ n2 ++ n3)
        where
        [v1, v2, v3] = splitOn " " line
        (vv1, n1) = doVertex v1
        (vv2, n2) = doVertex v2
        (vv3, n3) = doVertex v3
        doVertex str = (fromJust $ Map.lookup (read c1) vertices, fromJust $ Map.lookup (read c3) normals)
            where
            [c1, c2, c3] = splitOn "/" str

-- | Utility function to setup GLFW for graph drawing. Returns True on success.
setupGLFW :: IO Bool
setupGLFW = do
    setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg
    G.init

drawOrientation :: [GLfloat] -> [GLfloat] -> IO (Either String (Consumer (Quaternion GLfloat) IO ()))
drawOrientation verts norms = runEitherT $ do
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

