module BlockTexture.Builder.AdvancedPeripherals where

import qualified Data.Map as Map
import qualified Numeric.Noise.Extra as Noise
import qualified Numeric.Noise as Noise
import qualified Graphics.Image as Image
import qualified Graphics.Image.Extra as Image
import qualified Animation
import qualified Animation.Continuous as Animation
import qualified BlockTexture
import qualified BlockTexture.Builder as Builder

import Flow ((|>), (.>))
import Data.Functor ((<&>))
import Data.Functor.Flow ((<.>>))
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import Data.Fixed (mod')
import Control.Monad (join)
import Control.Monad.Require (require)
import Control.Lens ((.~), (%~), (??))
import Graphics.Image (Image)
import BlockTexture.Builder (Builder, requireSide, buildBlock)
import Animation (Animation)
import Numeric.Noise (lerp)
import qualified Animation.Continuous as ContinuousAnimation


squareParametricAt :: Double -> (Double, Double)
squareParametricAt i = if
  | i >= 0.00, i < 0.25 -> (-0.5 + i * 4, -0.5)
  | i >= 0.25, i < 0.50 -> (0.5, -0.5 + (i - 0.25) * 4)
  | i >= 0.50, i < 0.75 -> (0.5 - (i - 0.5) * 4, 0.5)
  | i >= 0.75, i <= 1.00 -> (-0.5, 0.5 - (i - 0.75) * 4)
  | otherwise -> error
    $ "the parameter to the curve should be between 0.0 and 1.0 but "
    ++ show i ++ " is not within that range"

indicatorsParametric :: Double -> (Double, Double)
indicatorsParametric i = if
  | i >= 0.00, i < 0.50 -> (3, 8 + i * 5)
  | i >= 0.50, i <= 1.00 -> (5, 8 + (i - 0.5) * 5)
  | otherwise -> error
    $ "the parameter to the curve should be between 0.0 and 1.0 but "
    ++ show i ++ " is not within that range"

glowingMask
  :: [(Int, Int)]
  -> Noise.Seed
  -> Animation.Continuous Image.VS Image.HSIA Double
glowingMask mask seed = Animation.MkContinuous \t -> let ?t = t in
  mask
    |> map (\(x, y) -> let ?x = x; ?y = y in
      ( (x, y)
      , Image.PixelHSIA (noise 0) 1 (noise 1) 1
      )
    )
    |> Map.fromList
    |> Image.fromMap Image.transparentPixel
    |> Image.canvasSize (Image.Fill Image.transparentPixel) (16, 16)
  where
    noise seedOffset = Noise.noise3At
      Noise.perlin3
      (seed + seedOffset)
      (sin (2 * pi * ?t) ** 2 * 4)
      (fromIntegral ?x / 16 * 4.8)
      (fromIntegral ?y / 16 * 4.8)

flowingMask
  :: [(Int, Int)]
  -> Noise.Seed
  -> Animation.Continuous Image.VS Image.HSIA Double
flowingMask mask seed = Animation.MkContinuous \t -> let ?t = t in
  mask
    |> map (\(x, y) -> let ?x = x; ?y = y in
      ( (x, y)
      , Image.PixelHSIA (noise 0) 1 (noise 1) 1
      )
    )
    |> Map.fromList
    |> Image.fromMap Image.transparentPixel
    |> Image.canvasSize (Image.Fill Image.transparentPixel) (16, 16)
  where
    noise seedOffset = Noise.toSphere
      Noise.perlin3
      (seed + seedOffset)
      1
      (fromIntegral ?x / 16 * 0.8 + ?t)
      (fromIntegral ?y / 16 * 0.8)

glowingFrameGeneric
  :: Int
  -> Noise.Seed
  -> Animation.Continuous Image.VS Image.HSIA Double
glowingFrameGeneric size seed = Animation.MkContinuous \t -> let ?t = t in
  [ 0 .. resolution ]
    |> map (/ resolution)
    |> map (\i -> let ?i = i in
      ( squareParametricAt i |> both rescale
      , Image.PixelHSIA (noise 0) 1 (noise 0) 1
      )
    )
    |> Map.fromList
    |> Image.fromMap Image.transparentPixel
  where
    resolution = 40

    rescale :: Double -> Int
    rescale = round . (+ 7.5) . (* (fromIntegral size - 0.5))

    noise seedOffset = Noise.toTorusParametric
      Noise.perlin3
      (seed + seedOffset)
      0.4
      ?t
      parametric (mod' (?t + ?i) 1)

    parametric = squareParametricAt .> both (* 1.8)

glowingFrame
  :: Noise.Seed
  -> Animation.Continuous Image.VS Image.HSIA Double
glowingFrame = glowingFrameGeneric 10

glowingIndicators
  :: Noise.Seed
  -> Animation.Continuous Image.VS Image.HSIA Double
glowingIndicators seed = Animation.MkContinuous \t -> let ?t = t in
  [ 0 .. 10 ]
    |> map (/ 10)
    |> map (\i -> let ?i = i in
      ( indicatorsParametric i |> both round
      , Image.PixelHSIA (noise 0) 1 (noise 1) 1
      )
    )
    |> Map.fromList
    |> Image.fromMap Image.transparentPixel
  where
    noise seedOffset = Noise.toSphere
      Noise.perlin3
      (seed + seedOffset)
      1
      ?i
      ?t

repeatSingleFrameCommon
  :: Builder.DefaultImageParams Image
  -> Builder.DefaultImageParams Animation
repeatSingleFrameCommon
  = Animation.fromAtlas (16, 16) 16 True
  .> Animation.repeat 16
  .> fromMaybe (error "impossible: 16 > 0")

modifySides
  :: String
  -> [( Maybe String
      , Noise.Seed
        -> Builder.DefaultImageParams Image
        -> Builder.DefaultImageParams BlockTexture.BlockFaceTexture
      )]
  -> (String, Builder)
modifySides blockName faces =
  let ?blockName = blockName in
  ( ?blockName
  , faces
    |> map (\(faceType, f) -> faceType
      <&> requireSide
      |> fromMaybe (require blockName)
      <.>> flip f
      <.>> fmap (fromMaybe "" faceType, )
    )
    |> sequence
    <.>> (??)
    <.>> buildBlock
    |> join
  )

adjustColor
  :: ( Double ->
       Double ->
       Double ->
       Double ->
       (Double, Double, Double, Double)
     )
  -> Animation.Continuous Image.VS Image.HSIA Double
  -> Builder.DefaultImageParams Animation.Continuous
adjustColor f = ContinuousAnimation.at %~ fmap (Image.map
  $ \(Image.PixelHSIA h s i a) ->
    let (h', s', i', a') = f h s i a
    in Image.toPixelRGBA (Image.PixelHSIA h' s' i' a')
  )

geoScanner :: (String, Builder)
geoScanner = modifySides "geo_scanner"
  let
    baseColor = adjustColor \_ _ i a
      -> (0.48, 0.82, lerp 0.55 0.9 i, a)
  in
  [ ( Just "top"
    , \seed
      -> repeatSingleFrameCommon
      .> Animation.overlayWithContinuous
        (glowingFrame seed |> baseColor)
      .> BlockTexture.Animated
    )
  , ( Just "front", const
    $ Animation.fromAtlas (16, 16) 2 True
    .> BlockTexture.Animated
    )
  , ( Nothing
    , \seed
      -> repeatSingleFrameCommon
      .> Animation.overlayWithContinuous
        (glowingIndicators seed |> baseColor)
      .> BlockTexture.Animated
    )
  ]

blockReader :: (String, Builder)
blockReader = modifySides "block_reader"
  let
    sideArrowMask :: [(Int, Int)]
    sideArrowMask
      = [(x, y) | x <- [ 5 .. 10 ], y <- [ 7, 8 ]]
      ++ [(x, y) | x <- [ 6, 7 ], y <- [ 6, 9 ]]
      ++ [(7, 5), (7, 10)]

    baseColor = adjustColor \h _ i a
      -> (mod' (0.025 + lerp (-0.01) 0.02 h) 1, 0.45, lerp 0.65 0.85 i, a)

    side angle seed
      = repeatSingleFrameCommon
      .> Animation.overlayWithContinuous
        (glowingFrame seed |> baseColor)
      .> Animation.overlayWithContinuous
        ( flowingMask sideArrowMask seed
          |> baseColor
          |> ContinuousAnimation.at %~ fmap
            (foldl (.) id (replicate angle Image.rotate90))
        )
      .> (Animation.delay .~ 4)
      .> BlockTexture.Animated
  in
  [ ( Just "west", side 0 )
  , ( Just "up", side 1 )
  , ( Just "east", side 2 )
  , ( Just "down", side 3 )
  , ( Just "north", \seed
    -> Animation.fromAtlas (16, 16) 4 True
    .> Animation.repeat 8
    .> fromMaybe (error "impossible: 8 > 0")
    .> Animation.overlayWithContinuous
      (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "south", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  ]

chatBox :: (String, Builder)
chatBox = modifySides "chat_box"
  let
    baseColor = adjustColor \_ _ i a
      -> (0, 0, lerp 0.7 0.95 i, a)
    occasionalColorOnTheFrame hue seed
      = Animation.overlayWithContinuous
      ( glowingFrame seed |> adjustColor \_ _ i _ ->
        ( hue
        , 1
        , 0.73
        , if i > 0.5
          then lerp 0.2 1 ((i - 0.5) * 2)
          else 0
        )
      )
  in
  [ ( Nothing, \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> (Animation.delay .~ 4)
    .> BlockTexture.Animated
    )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> occasionalColorOnTheFrame 0.9 seed
    .> occasionalColorOnTheFrame 0.55 (seed + 1)
    .> (Animation.delay .~ 8)
    .> BlockTexture.Animated
    )
  , ( Just "front", const
    $ Animation.fromAtlas (16, 16) 2 False
    .> BlockTexture.Animated
    )
  ]

colonyIntegrator :: (String, Builder)
colonyIntegrator = modifySides "colony_integrator"
  let
    baseColor = adjustColor \_ s i a
      -> (0.1, lerp 0.41 0.54 s, lerp 0.51 0.72 i, a)
  in
  [ ( Nothing, \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "front", const
    $ Animation.fromAtlas (16, 16) 2 False
    .> BlockTexture.Animated
    )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  ] 

energyDetector :: (String, Builder)
energyDetector = modifySides "energy_detector"
  let
    mkMask :: [Int] -> [(Int, Int)]
    mkMask exc = 
      [ (x, y)
      | x <- [ 0 .. 5 ]
      , y <- [ 0 .. 5 ]
      , even (x `div` 2 + y `div` 2)
      ]
      |> fmap (both (+ 5))
      |> filter (`notElem` [(x, y) | x <- exc, y <- exc])

    baseColorF f _ s i a =
      ( mod' (lerp 0 (-0.05) i) (1 - 1e-10)
      , lerp 0.67 0.76 s
      , f i
      , a
      )

    baseColor = adjustColor (baseColorF (lerp 0.20 0.44))
  in
  [ ( Nothing, \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "back", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous
      (glowingMask (mkMask [6, 9]) seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "east", const
    $ Animation.fromAtlas (16, 16) 2 False
    .> BlockTexture.Animated
    )
  , ( Just "front", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous 
      (glowingMask (mkMask [5, 10]) seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous
      ( glowingFrame seed
      |> ContinuousAnimation.at %~ \frameAt t -> Image.map
        (\(Image.PixelHSIA h s i a) ->
          let
            (h', s', i', a') = baseColorF
              (lerp (0.06 + (0.5 + sin (pi * 2 * t) / 2) * 0.25) 0.45)
              h s i a
          in Image.toPixelRGBA $ Image.PixelHSIA h' s' i' a'
        )
        (frameAt t)
      )
    .> BlockTexture.Animated
    )
  ]

environmentDetector :: (String, Builder)
environmentDetector = modifySides "environment_detector"
  let
    baseColor = adjustColor \_ s i a ->
      (0.33, lerp 0.7 0.35 s, lerp 0.31 0.53 i, a)
  in
  [ ( Nothing, \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "front", const
    $ Animation.fromAtlas (16, 16) 4 False
    .> BlockTexture.Animated
    )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  ]

inventoryManager :: (String, Builder)
inventoryManager = modifySides "inventory_manager"
  let
    baseColor = adjustColor \_ s i a ->
      (0.47, lerp 0.8 0.7 s, lerp 0.4 0.8 i, a)
  in
  [ ( Nothing, \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "front", const BlockTexture.Static )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  ]

meBridge :: (String, Builder)
meBridge = modifySides "me_bridge"
  let
    baseColor = adjustColor \_ s i a ->
      (0.77, lerp 0.8 0.7 s, lerp 0.7 1 i, a)
    occasionalWhiteBlobs seed
      = Animation.overlayWithContinuous
      ( glowingFrame seed |> adjustColor \_ _ i _ ->
        ( 0
        , 0
        , 1
        , if i > 0.5
          then lerp 0.6 1 ((i - 0.5) * 2)
          else 0
        )
      )
  in
  [ ( Nothing, \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Nothing, const BlockTexture.Static )
  , ( Just "front", const
    $ Animation.fromAtlas (16, 16) 2 False
    .> BlockTexture.Animated
    )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> occasionalWhiteBlobs seed
    .> BlockTexture.Animated
    )
  ]

nbtStorage :: (String, Builder)
nbtStorage = modifySides "nbt_storage"
  let
    baseColor = adjustColor \_ s i a ->
      (lerp 0.08 0.17 i, lerp 0.34 0.2 s, lerp 0.7 1 i, a)
  in
  [ ( Nothing, \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "front", const
    $ Animation.fromAtlas (16, 16) 2 False
    .> BlockTexture.Animated
    )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  ]

playerDetector :: (String, Builder)
playerDetector = modifySides "player_detector"
  let
    baseColor = adjustColor \_ s i a ->
      (0.33, lerp 0.99 0.9 s, lerp 0.4 0.9 i, a)
  in
  [ ( Just "front", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "top", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "side", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingIndicators seed |> baseColor)
    .> BlockTexture.Animated
    )
  ]

redstoneIntegrator :: (String, Builder)
redstoneIntegrator = modifySides "redstone_integrator"
  let
    baseColor = adjustColor \_ _ i a ->
      (0, 0.93, lerp 0.45 0.95 i, a)
    tripleBarMask =
      [ (3 + x, 3 + y * 2)
      | y <- [ 0 .. 2 ]
      , x <- [ 0 .. (2 - y) ]
      ]
  in
  [ ( Just "bottom", \seed
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous
      (glowingFrameGeneric 6 seed |> baseColor)
    .> Animation.overlayWithContinuous
      (glowingFrameGeneric 10 (seed + 1) |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "front", \seed
    -> Animation.fromAtlas (16, 16) 2 False
    .> Animation.overlayWithContinuous
      (flowingMask tripleBarMask seed |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "side", \((+ 10) -> seed)
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous
      (glowingFrameGeneric 6 seed |> baseColor)
    .> Animation.overlayWithContinuous
      (glowingFrameGeneric 10 (seed + 1) |> baseColor)
    .> BlockTexture.Animated
    )
  , ( Just "top", \((+ 20) -> seed)
    -> repeatSingleFrameCommon
    .> Animation.overlayWithContinuous (glowingFrame seed |> baseColor)
    .> BlockTexture.Animated
    )
  ]

allBuilders :: [(String, Builder)]
allBuilders =
  [ geoScanner
  , blockReader
  , chatBox
  , colonyIntegrator
  , energyDetector
  , environmentDetector
  , inventoryManager
  , meBridge
  , nbtStorage
  , playerDetector
  , redstoneIntegrator
  ]
