import Graphics.UI.GLUT

myPoints :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
myPoints n = [ (sin (2*pi*k/n), cos (2*pi*k/n), 0) | k <- [1..n] ]

main :: IO()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    displayCallback $= display
    mainLoop
    
display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    renderPrimitive TriangleFan $
        mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) (myPoints 30)
    flush