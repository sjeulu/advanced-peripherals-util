module Graphics.Image.Processing.Extra where

import qualified Graphics.Image as Image
import qualified Graphics.Image.Interface as Image
import qualified Graphics.Image.Extra as Image

import Graphics.Image (Image)
import Data.Biapplicative (Biapplicative(biliftA2))


overlay
  :: Image.AlphaSpace cs e
  => Floating e
  => Image.Array arr cs e
  => Image arr cs e
  -> Image arr cs e
  -> Image arr cs e
overlay topImage bottomImage = Image.traverse2
  topImage
  bottomImage
  (biliftA2 max max)
  \f g ij ->
    let
      top = Image.safeView topImage f ij
      bottom = Image.safeView bottomImage g ij
      aT = Image.getAlpha top
      aB = Image.getAlpha bottom
      baseTop = Image.dropAlpha top
      baseBot = Image.dropAlpha bottom
      blend cT cB = cT * aT + cB * (1 - aT)
      blendedBase = Image.liftPx2 blend baseTop baseBot
      aOut = aT + aB * (1 - aT)
    in
      Image.addAlpha aOut blendedBase
