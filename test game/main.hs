import FreeGame

gameLoop :: Bitmap -> Double -> Double -> Game ()
gameLoop image x y = do
    translate (V2 x y) $ bitmap image
    tick
    whenM (keyPress KeyLeft) $ gameLoop image (x-10) y
    whenM (keyPress KeyUp) $ gameLoop image x (y-10)
    whenM (keyPress KeyRight) $ gameLoop image (x+10) y
    whenM (keyPress KeyDown) $ gameLoop image x (y+10)
    unlessM (keyDown KeyEscape) $ gameLoop image x y

main = runGame Windowed (Box (V2 0 0) (V2 800 600)) $ do
    hideCursor
    charBitmap <- readBitmap "character.png"
    gameLoop charBitmap 0 0