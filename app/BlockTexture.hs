module BlockTexture where

import qualified Graphics.Image as Image
import qualified Graphics.Image.Extra as Image
import qualified Animation

import Flow ((.>))
import Graphics.Image (Image)
import System.FilePath ((</>))
import Animation (Animation)
import Control.Monad (forM_)
import Control.Lens (over, makeLenses, Field2 (_2), Each (each), (%~))
import Data.Function (applyWhen)


data BlockFaceTexture arr cs e
  = Static (Image arr cs e)
  | Animated (Animation arr cs e)

writeFace
  :: Image.WritablePlus arr cs e
  => String
  -> FilePath
  -> BlockFaceTexture arr cs e
  -> IO ()
writeFace fileNamePrefix path = \case
  Static texture
    -> Image.writeImage
      (path </> fileNamePrefix <> ".png")
      texture
  Animated animation
    -> Animation.write fileNamePrefix path animation


data BlockTexture arr cs e = MkBlockTexture
  { _name :: String
  , _faces :: [ (String, BlockFaceTexture arr cs e) ]
  }

makeLenses ''BlockTexture

mapFaces
  :: (Image arr cs e -> Image arr' cs' e')
  -> (BlockTexture arr cs e -> BlockTexture arr' cs' e')
mapFaces f = (faces . each . _2) %~ \case
  Static image -> Static (f image)
  Animated animation -> Animated (Animation.mapFrames f animation)

faceFileName :: BlockTexture arr cs e -> String -> String
faceFileName blockTexture faceType = applyWhen (faceType /= "")
  (<> ("_" <> faceType))
  (_name blockTexture)

writeBlock
  :: Image.WritablePlus arr cs e
  => FilePath
  -> BlockTexture arr cs e
  -> IO ()
writeBlock path blockTexture = forM_
  (_faces blockTexture)
  (uncurry \faceType -> writeFace
    (faceFileName blockTexture faceType)
    path
  )

writeBlockAsGIFs
  :: Image.Writable [(Image.GifDelay, Image arr cs e)] (Image.Seq Image.GIF)
  => FilePath
  -> BlockTexture arr cs e
  -> IO ()
writeBlockAsGIFs path blockTexture = forM_
  (_faces blockTexture)
  (\(faceType, texture) -> case texture of
    Animated a -> Animation.writeAsGIF
      path
      (faceFileName blockTexture faceType)
      a
    Static _ -> pure ()
  )
