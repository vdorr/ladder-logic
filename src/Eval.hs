{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-incomplete-patterns
                     -fwarn-unused-binds
                     -fwarn-unused-imports #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Eval where
#if 0
import UI.NCurses

{-

  |_▅_▅▅▅_
  |_▅▅_▅__
  ╵ 
 ▕10

│ │ 
┼───
│ │ 

   │
V1 │_▂▃▄▅▆▇█________
  0|   ╵   ╵   ╵   ╵4s
V0 │_▂▃▄▅▆▇█▇▆▅▄▃▂__

   │
V1_│_▂▃▄▅▆▇█________
V0_│_▂▃▄▅▆▇█▇▆▅▄▃▂__
   ╵   ╵   ╵   ╵   ╵
              1s/div

-}


trace' :: Num a => [a] -> [String]
trace' = undefined

trace :: [Bool] -> String
trace = undefined

plot :: String -> Int -> Int -> [Bool] -> String
plot = undefined


xxx v = fmap (bar.scale) v
	where
	l = minimum v
	h = maximum v
	q = fromIntegral (length charset - 1) / (h - l)
	scale y = round $ (y - l) * q
-- 	bar :: Int -> Char
	bar n = charset !! n
	charset = "_▂▃▄▅▆▇█"

-- main = do
-- 	print here
-- 	let x = [0..40]
-- 	let y :: [Float] = fmap (((-)1).sin) $ fmap (*(pi/8)) x
-- -- 	print (here, y)
-- -- 	print (here, xxx y)
-- 	putStrLn $ xxx y
-- 

x0 = [0..40]
y0 :: [Float] = fmap (((-)1).sin) $ fmap (*(pi/8)) x0

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
--         drawString "Hello world!"
        drawString $ xxx y0
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
#endif
