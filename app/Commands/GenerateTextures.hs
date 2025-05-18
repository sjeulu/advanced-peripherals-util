module Commands.GenerateTextures
  ( CmdOpts
  , parseCmd
  , run
  )
  where

import qualified Data.Text as Text
import qualified Graphics.Image as Image
import qualified Graphics.Image.Interface as Image
import qualified BlockTexture
import qualified BlockTexture.Builder as Builder
import qualified BlockTexture.Builder.AdvancedPeripherals as AdvancedPeripheralBuilders

import NeatInterpolation (text)
import Flow ((|>))
import Data.Function (applyWhen)
import Data.Functor ((<&>))
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Graphics.Image.Extra ()
import Graphics.Image.IO.Formats ()
import Options.Applicative
  ( Mod, CommandFields
  , info, progDesc
  , option, str
  , command, long, short, value, help, switch
  )
import System.FilePath ((</>))
import System.Process (callProcess)
import Data.Maybe (fromMaybe)
import System.Directory (setCurrentDirectory)


data CmdOpts = MkCmdOpts
  { assetsPath :: FilePath
  , outputPath :: FilePath
  , inject :: Maybe FilePath
  , gifOnly :: Bool
  , upscale :: Bool
  } deriving Show


parseCmd :: (CmdOpts -> a) -> Mod CommandFields a
parseCmd f = command "generate-textures" $ info
  ( fmap f $ MkCmdOpts
    <$> option str
      ( long "assets-path"
      <> short 'a'
      <> value
        ( "src" </> "main" </> "resources" </> "assets"
        </> "advancedperipherals" </> "textures" </> "block"
        )
      <> help (Text.unpack [text|
        Path to the directory where the original block textures are stored
      |])
      )
    <*> option str
      ( long "output-path"
      <> short 'o'
      <> value ("src" </> "generated" </> "block-textures")
      <> help (Text.unpack [text|
        Where to the directory where the output "assets" directory resides e.g.
        `src/generated/resources`.
      |])
      )
    <*> option (Just <$> str)
      ( long "inject"
      <> short 'i'
      <> value Nothing
      <> help (Text.unpack [text|
        Injects the generated textures into the specified .jar file, replacing
        existing ones with the same names
      |])
      )
    <*> switch
      ( long "gif-only"
      <> short 'g'
      <> help (Text.unpack [text|
        Exports only animated block faces as GIF files.
        Useful for previewing the animations
      |])
      )
    <*> switch
      ( long "upscale"
      <> short 'u'
      <> help (Text.unpack [text|
        Makes the exported textures 100 times bigger which makes them easier
        to view in some scenarios
      |])
      )
  )
  ( progDesc "Generates the procedurally animated textures for blocks." )


run :: CmdOpts -> IO ()
run opts = do
  let
    assetsPath' = assetsPath opts
    outputPath' = outputPath opts
  putStrLn $ "Using " ++ assetsPath' ++ " as the assets directory"
  putStrLn $ "Using " ++ outputPath' ++ " as the output path"

  forM_ AdvancedPeripheralBuilders.allBuilders
    \(Text.pack -> name, builder) -> do
      runExceptT (Builder.run assetsPath' builder) >>= \case
        Right blockTexture -> do
          let
            blockTexture' = applyWhen (upscale opts)
              ( BlockTexture.mapFaces $ Image.scale
                Image.Nearest
                Image.Edge
                (10, 10)
              )
              blockTexture
          putStrLn $ Text.unpack [text|
            Writing textures for $name
          |]
          if gifOnly opts
            then BlockTexture.writeBlockAsGIFs
              outputPath'
              ( BlockTexture.mapFaces
                (Image.map (fmap Image.toWord8))
                blockTexture'
              )
            else BlockTexture.writeBlock outputPath' blockTexture'
        Left (Text.pack -> err) -> putStrLn $ Text.unpack [text|
          Could not run the texture builder for $name: $err
        |]

  inject opts
    <&> (\jarPath -> do
      setCurrentDirectory outputPath'
      callProcess "zip" [ jarPath, "-ur", "assets" ]
    )
   |> fromMaybe (pure ())
