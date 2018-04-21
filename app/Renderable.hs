module Renderable where

import Graphics.Gloss

class Renderable a where
  -- This typeclass is for anything that is renderable
  -- It should be used to render almost any entity
  render :: a -> Picture
