module Animation.Continuous where

import Control.Lens ((%~), makeLenses)
import Graphics.Image (Image)

newtype Continuous arr cs e = MkContinuous
  { _at :: Double -> Image arr cs e
  }

makeLenses ''Continuous

map
  :: (Image arr cs e -> Image arr' cs' e')
  -> Continuous arr cs e -> Continuous arr' cs' e'
map f = at %~ fmap f
