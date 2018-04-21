{-# LANGUAGE FlexibleInstances #-}
module Physics where

import Renderable
import qualified Graphics.Gloss as G

newtype Degrees = Degrees Float deriving (Show, Eq)
newtype Radians = Radians Float deriving (Show, Eq)

degreesToRadians :: Degrees -> Radians
degreesToRadians (Degrees deg) = Radians $ pi / 180 * deg

radiansToDegrees :: Radians -> Degrees
radiansToDegrees (Radians rad) = Degrees $ rad * 180 / pi

data Vector a = Vector { x :: a
                       , y :: a } deriving (Show, Eq)

origin :: Num a => Vector a
origin = Vector 0 0

numToVector :: Num a => a -> Vector a
numToVector a = Vector a a

instance Num Radians where
  (Radians a) + (Radians b) = Radians (a + b)
  (Radians a) * (Radians b) = Radians (a * b)

instance Num a => Num (Vector a) where
  (Vector ax ay) + (Vector bx by) = Vector (ax + bx) (ay + by)
  (Vector ax ay) * (Vector bx by) = Vector (ax * bx) (ay * by)

  negate (Vector x y) = Vector (negate x) (negate y)
  abs (Vector x y) = Vector (abs x) (abs y)

instance Fractional a => Fractional (Vector a) where
    (Vector ax ay) / (Vector bx by) = Vector (ax / bx) (ay / by)

instance Renderable Shape where
  render (Circle r) = G.circle (realToFrac r)
  render (Text cont) = G.scale 0.1 0.1 $ G.text cont
  render (Rectangle w h) = G.rectangleWire (realToFrac w) (realToFrac h)

instance Renderable a => Renderable (Entity a) where
  render Entity{position=(Vector x y), shape=shape, angle=angle} =
    G.translate (realToFrac x) (realToFrac y) $
    G.rotate    ang $
    render shape
    where (Degrees ang) = radiansToDegrees angle

data Shape = Circle Float
           | Rectangle Float Float
           | Text String
           deriving (Show, Eq)

gravitationConstant :: Float
gravitationConstant = 6.67384 * 10 ^^ negate 11

dist :: Floating a => Vector a -> Vector a -> a
dist (Vector ax ay) (Vector bx by) = sqrt ( (ax - bx)^^2 + (ay - by)^^2)

normalize :: (Ord a, Floating a) => Vector a -> Vector a
normalize vec@(Vector x y) = Vector (x / mag) (y / mag)
    where mag = max 1 $ dist vec origin 

{-gforce * normalize $ pb - pa-}

-- This is a scalar for the gforce between two entities
gforce :: Entity a -> Entity a -> Float
gforce a@Entity{mass=ma, position=pa} b@Entity{mass=mb, position=pb} =
  gravitationConstant * (ma * mb / (dist pa pb ^^ 2))

applyGravity :: Float -> Entity a -> Entity a -> Entity a
applyGravity dt a@Entity{position=pa} b@Entity{position=pb} = applyForce dt (numToVector (gforce a b) * (normalize $ pb - pa)) a

applyForce :: Float -> Vector Float -> Entity a -> Entity a
applyForce dt force e@Entity{velocity=velocity, mass=mass} = e{velocity=velocity + force / (numToVector mass) * (numToVector dt) }

applyImpulse :: Vector Float -> Entity a -> Entity a
applyImpulse impulse e@Entity{velocity=velocity} = e{velocity=velocity + impulse}

updateEntity :: Float -> Entity a -> Entity a
updateEntity dt e@Entity{ position=p@(Vector x y), velocity=v, angle=angle, angularMomentum=am} =
  e{position=p+(v*Vector dt dt), angle=angle+am*Radians dt}

data Entity a = Entity { id              :: Int
                       , position        :: Vector Float
                       , velocity        :: Vector Float
                       , mass            :: Float
                       , angle           :: Radians
                       , angularMomentum :: Radians
                       , shape           :: a } deriving (Show)

instance Eq (Entity a) where
    (Entity a _ _ _ _ _ _) == (Entity b _ _ _ _ _ _) = a == b
