module V0 where

import qualified Overloaded.Lists

data Vec a = Nil
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Vec where
    pure _  = Nil
    _ <*> _ = Nil

instance Overloaded.Lists.Nil (Vec a) where
    nil = Nil
