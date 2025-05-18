module BlockTexture.Builder where

import qualified Data.Text as Text
import qualified Numeric.Noise as Noise
import qualified Graphics.Image as Image
import qualified System.Directory as Directory

import NeatInterpolation (text)
import Data.Functor ((<&>))
import Graphics.Image (Image)
import BlockTexture (BlockTexture (MkBlockTexture), BlockFaceTexture)
import Control.Monad.Except (MonadError(throwError), ExceptT)
import System.FilePath ((</>))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Require (Require, require, resolve)
import qualified System.Random as Random


type DefaultImageParams a = a Image.VS Image.RGBA Double

type Builder = Require
  String
  (DefaultImageParams Image)
  (Noise.Seed -> DefaultImageParams BlockTexture)

type Error = String


requireSide
  :: (?blockName :: String)
  => String
  -> Require String (Image arr cs e) (Image arr cs e)
requireSide sideName = require $ ?blockName ++ "_" ++ sideName

buildBlock
  :: (?blockName :: String)
  => (Noise.Seed -> [(String, DefaultImageParams BlockFaceTexture)])
  -> Builder
buildBlock faces = pure $ \seed -> MkBlockTexture ?blockName (faces seed)

run
  :: FilePath
  -> Noise.Seed
  -> Builder
  -> ExceptT Error IO (BlockTexture Image.VS Image.RGBA Double)
run assetsDir seed builder = do
  f <- flip resolve builder \textureName -> do
    let
      texturePath = assetsDir </> textureName <> ".png"
      texturePath' = Text.pack texturePath
    yes <- lift $ Directory.doesFileExist texturePath
    if yes
      then lift (Image.readImage texturePath) >>= \case
        Right image -> (pure image)
        Left (Text.pack -> err) -> throwError $ Text.unpack [text|
          could not read texture $texturePath': $err
        |]
      else throwError $ Text.unpack [text|
        could not find texture $texturePath'
      |]
  pure (f seed)
