module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Physics
import Renderable
import Control.Monad.IO.Class
import Data.List

window :: Display
window = InWindow "Physics Simulation" (640, 480) (10, 10)

background :: Color
background = white

data Model = Model { entities :: [Entity Shape]
                   , trace    :: [Picture] } deriving Show

instance Renderable Model where
  render Model{entities=e, trace=trace} = pictures $ map render e ++ trace

renderIO :: Model -> IO Picture
renderIO = return . render

handleEvent :: Event -> Model -> IO Model
handleEvent _ = return

getPosition :: Entity a -> String
getPosition Entity{position=position, Physics.id=id} = '#' : show id ++ " -> " ++ show position

others :: [a] -> [[a]]
others xs = ($ []) . foldr f (const []) $ zip (drop 1 . tails $ xs) xs
  where f (as, a) b = \front -> (front ++ as) : b (front ++ [a])

othersWithSelf :: [a] -> [(a, [a])]
othersWithSelf = zip <$> Prelude.id <*> others

tick :: Float -> Model -> IO Model
tick dt model@Model{entities=entities, trace=trace} = do

    let withGravity = map (\(e, rest) -> foldr (flip (applyGravity dt)) e rest) . othersWithSelf
    let withUpdate = map (updateEntity dt)

    let newEntities = withUpdate . withGravity $ entities

    let newstuff = zipWith (\a@Entity{position=(Physics.Vector ax ay)} x@Entity{position=(Physics.Vector bx by)} -> line [(ax, ay), (bx, by)]) entities newEntities

    return model{entities=newEntities, trace=newstuff++trace}

startModel :: Model
startModel = Model [ Entity 0 (Vector (negate 200) 0) (Vector 0          20) (1.4 * 10 ^^ 16)  (Radians 0) (Radians 0)       (Physics.Circle 20)
                   , Entity 1 (Vector 200          0) (Vector 0 (negate 20)) (1.4 * 10 ^^ 16)  (Radians 0) (Radians 0)       (Physics.Circle 20)
                   , Entity 3 (Vector 200        200) (Vector (negate 25) 25) (4.6 * 10 ^^ 10) (Radians 0) (Radians $ pi/16) (Physics.Circle 5)
                   ] [] 


main :: IO ()
main = playIO window white 20 startModel renderIO handleEvent tick

