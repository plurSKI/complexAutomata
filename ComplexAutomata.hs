import Graphics.GD
import Complex
import Data.Bits

type Position = (Int, Int)
type Board = [(Position, Complex Double)]
data Automata = Automata { location :: Position, value :: Complex Double, func :: (Complex Double -> Complex Double) } 
type RgbColor   = (Int, Int, Int)
type ColorPoint = (Int, Int, RgbColor)

width  = 201
height = 201

initialBoard :: Board
initialBoard = [ ((x, y), (fromIntegral x) :+ (fromIntegral y)) | x <- [0..width], y <- [0..height] ]

pAuto x y = Automata (x,y) (0 :+ 0) f
  where f z  = (z - (50 :+ 50))^3 / (sin (z - (50 :+ 50)))
 -- where f z = exp z
 -- where f x = (1/4) * x ^ 3 - (1/3) * x ^ 2 + (1/2) * x - (1 :+ 1)  

complexMod :: Complex Double -> Complex Double
complexMod cNum = ((fromIntegral x') + x'') :+ ((fromIntegral y') + y'')
  where (x, y)      = (realPart cNum, imagPart cNum)
        (x', y')    = ((floor x) `mod` width , (floor y) `mod` height)
        (x'',y'')   = (x - (fromIntegral (floor x)), y - (fromIntegral (floor y)))
  

moveAutomata :: (Board, Automata) -> (Board, Automata)
moveAutomata (b, a) = (newBoard, newAuto)
  where currentTile = head $ filter (\x -> location a == fst x) b
        newValue    = complexMod $ (func a) (snd currentTile)
        newPos      = (calcPos realPart width, calcPos imagPart height)
        newAuto     = Automata (newPosX, newPosY) newValue (func a)
        newTile     = (fst currentTile, newValue)
        newBoard    = [newTile] ++ filter (\x -> fst currentTile /= fst x) b 
        calcPos f m = floor (f newValue) `mod` m 
        oldX        = fst (location a)
        oldY        = snd (location a)
        newPosX | fst newPos < oldX = oldX - 1 
                | fst newPos > oldX = oldX + 1
                | otherwise         = oldX
        newPosY | snd newPos < oldY = oldY - 1 
                | snd newPos > oldY = oldY + 1
                | otherwise         = oldY
         

rgbToCInt :: (Int, Int, Int)-> Color
rgbToCInt (r,g,b) = toEnum $ rotateL r 16 + rotateL g 8 + b 

drawPoints img scale ((x,y), c) = drawFilledRectangle (x',y') (x' + scale,y' + scale) (getColor c) img
  where x' = x*scale
        y' = y*scale
        getColor cNum = rgbToCInt (floor (realPart cNum),floor (imagPart cNum),127)

boardImage :: Board -> Int -> String -> IO()
boardImage b scale filename = do
  img <- newImage (width*scale, height*scale)
  mapM_ (drawPoints img scale) b 
  savePngFile ("data/" ++ filename ++ ".png") img

computeMoves :: (Board, [Automata]) -> Int -> IO ()
computeMoves (_, _) 10000 = putStrLn "Done" 
computeMoves (b, xs) x   = do
   putStrLn $ show x 
   mapM_ print (map location xs)
   boardImage b 5 (show x)
   let newVals = foldl (\list x -> list ++ [moveAutomata (fst (last list), x)]) [ moveAutomata (b,head xs)] (tail xs)
   computeMoves (fst (last newVals), map snd newVals) (x + 1)

main = computeMoves (initialBoard, [pAuto 50 50, pAuto 50 100, pAuto 50 150, pAuto 100 50, pAuto 100 100, pAuto 100 150, pAuto 150 50, pAuto 150 100, pAuto 150 150]) 0


