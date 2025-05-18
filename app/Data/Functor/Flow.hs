module Data.Functor.Flow ((<.>>)) where

import Flow ((.>))

infixl 4 <.>>
(<.>>) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
a <.>> f = a .> fmap f
