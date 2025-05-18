module Graphics.Image.Extra where

import qualified Data.Map as Map
import qualified Graphics.Image as Image
import qualified Graphics.Image.Interface as Image

import Flow ((|>), (.>))
import Data.Map (Map)
import Graphics.Image (Image)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Tuple.Extra (both)
import Control.Conditional ((??))
import Data.Biapplicative (Biapplicative(biliftA2))
import Data.Foldable (maximumBy)


class 
  ( Image.Array Image.VS cs e
  , Image.Array arr cs e
  , Image.Writable (Image Image.VS cs e) Image.OutputFormat
  ) => WritablePlus arr cs e where

instance WritablePlus Image.VS Image.RGBA Double


fromMap
  :: Image.Array arr cs e
  => Image.Pixel cs e
  -> Map (Int, Int) (Image.Pixel cs e)
  -> Image arr cs e
fromMap emptyPixel m = if Map.null m
  then Image.fromLists []
  else Image.makeImage
    ( Map.keys m
      |> unzip
      |> both maximum
      |> swap
      |> both (+ 1)
    )
    \k -> fromMaybe emptyPixel (Map.lookup (swap k) m)

transparentPixel :: (Image.AlphaSpace cs e) => Image.Pixel cs e
transparentPixel = Image.addAlpha 0 (Image.promote 0)

safeView
  :: Image.Array arr cs e
  => Image.AlphaSpace cs e
  => Image arr cs e
  -> ((Int, Int) -> Image.Pixel cs e)
  -> (Int, Int)
  -> Image.Pixel cs e
safeView image f ij = Image.dims image
  |> biliftA2 (<) (<) ij
  |> uncurry (&&)
  |> f ij ?? transparentPixel
