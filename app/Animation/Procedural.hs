module Animation.Procedural where

import qualified Numeric.Noise as Noise
import qualified System.Random as Random

import Data.Functor ((<&>))
import Animation (Animation)


newtype Procedural arr cs e = MkProcedural
  { withSeed :: Noise.Seed -> Animation arr cs e
  }

instantiate
  :: Procedural arr cs e
  -> IO (Animation arr cs e)
instantiate a = Random.randomIO <&> withSeed a

