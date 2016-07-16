import Graphics.HGL 


-- Prelude Graphics.HGL> :t drawInWindow
-- drawInWindow :: Window -> Graphic -> IO ()
-- Prelude Graphics.HGL> :t runGraphics
-- runGraphics :: IO () -> IO ()
-- Prelude Graphics.HGL> :t withWindow
-- withWindow :: Title -> Size -> (Window -> IO a) -> IO a
-- Prelude Graphics.HGL> let g = ellipse (10,50) (50,10)

g = ellipse (10,10) (50,50)

main = runGraphics $ withWindow_ "Hello, world." (300,300) 
       $ \w -> do drawInWindow w g
                  getKey w


-- module Main where

-- import Graphics.HGL

-- main :: IO ()
-- main = runGraphics $
--        withWindow_ "Hello World Window" (300, 200) $ \ w -> do
--        drawInWindow w $ text (100, 100) "Hello World"
--        drawInWindow w $ ellipse (100, 80) (200, 180)
--        getKey w
       
