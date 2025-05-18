module Animation where

import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Graphics.Image as Image
import qualified Graphics.Image.Extra as Image
import qualified Graphics.Image.Processing.Extra as Image
import qualified Graphics.Image.Interface as Image

import Flow ((|>), (.>))
import NeatInterpolation (text)
import Data.Functor ((<&>))
import Data.Bifunctor (Bifunctor(bimap))
import Graphics.Image (Image)
import System.FilePath ((</>))
import Control.Monad (join)
import Control.Lens (makeLenses, over, set, (^.))
import Animation.Continuous (Continuous, at)

type NonEmptyList = NonEmptyList.NonEmpty


type Delay = Int

data Animation arr cs e = MkAnimation
  { _delay :: Delay
  , _interpolate :: Bool
  , _frames :: NonEmptyList (Image arr cs e)
  }

makeLenses ''Animation

mapFrames
  :: (Image arr cs e -> Image arr' cs' e')
  -> Animation arr cs e -> Animation arr' cs' e'
mapFrames = over frames . fmap

repeat :: Int -> Animation arr cs e -> Maybe (Animation arr cs e)
repeat n a = a
  |> _frames
  |> NonEmptyList.toList
  |> replicate n
  |> concat
  |> NonEmptyList.nonEmpty
  |> fmap (flip (set frames) a)

assembleAtlas
  :: Image.Array arr cs e
  => Animation arr cs e
  -> Image arr cs e
assembleAtlas = foldr1 Image.topToBottom . _frames

fromAtlas
  :: Image.Array arr cs e
  => (Int, Int)
  -> Delay
  -> Bool
  -> Image arr cs e
  -> Animation arr cs e
fromAtlas (frameWidth, frameHeight) delay' interpolate' atlas
  = Image.dims atlas
  |> bimap (`div` frameHeight) (`div` frameWidth)
  |> (\(rows, cols)
    -> [ 0 .. max 0 $ rows - 1 ] <&> (* frameHeight) .> \y
    -> [ 0 .. max 0 $ cols - 1 ] <&> (* frameWidth) .> \x
    -- ^ handles the edge case when the atlas is of size 0
    -> Image.crop
      (y, x)
      (Image.dims atlas |> bimap
        (min frameHeight . subtract y)
        (min frameWidth . subtract x)
        -- ^ in case the image doesn't tile perfectly
      )
      atlas
  )
  |> join
  |> MkAnimation delay' interpolate'

write
  :: Image.WritablePlus arr cs e
  => String
  -> FilePath
  -> Animation arr cs e
  -> IO ()
write fileNamePrefix path animation = do
  let
    imageName = fileNamePrefix <> ".png"
    delay' = Text.pack . show $ _delay animation
    interpolate' = Text.toLower . Text.pack . show $ _interpolate animation
  Image.writeImage
    (path </> imageName)
    (assembleAtlas animation)
  writeFile
    (path </> imageName <> ".mcmeta")
    (Text.unpack [text|
      {
        "animation": {
          "interpolate": $interpolate',
          "frametime": $delay'
        }
      }
    |])

writeAsGIF
  :: Image.Writable [(Image.GifDelay, Image arr cs e)] (Image.Seq Image.GIF)
  => String
  -> FilePath
  -> Animation arr cs e
  -> IO ()
writeAsGIF path fileNamePrefix animation = do
  let
    imageName = fileNamePrefix <> ".gif"
  _frames animation
    |> NonEmptyList.toList
    |> fmap (round @Double @Int $ fromIntegral (_delay animation) * 5, )
      -- GifDelay is expressed in increments of 0.01s while the "frametime"
      -- field of texture animations is 1tick = 0.05s
    |> Image.encode
      (Image.Seq Image.GIF)
      [Image.GIFSeqLooping Image.LoopingForever]
    |> ByteString.writeFile (path </> imageName)

overlayWithContinuous
  :: (Image.AlphaSpace cs e, Floating e, Image.Array arr cs e)
  => Continuous arr cs e
  -> Animation arr cs e
  -> Animation arr cs e
overlayWithContinuous c a = over frames
  ( NonEmptyList.zip [ 0 .. length' ]
  .> fmap \(i, frame) -> Image.overlay (c ^. at $ i / length') frame
  )
  a
  where length' = fromIntegral (length (_frames a))
