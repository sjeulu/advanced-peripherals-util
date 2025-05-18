module Data.Functor.Adjunction where

import Control.Arrow ((|||))


-- | Every functor in Haskell permits 'uncozipping'
-- (copied verbatim from adjunctions-4.4.3)
uncozipL :: Functor f => Either (f a) (f b) -> f (Either a b)
uncozipL = fmap Left ||| fmap Right
