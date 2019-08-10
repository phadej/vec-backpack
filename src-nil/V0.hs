module V0 where

data Vec a = Nil
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Vec where
    pure _  = Nil
    _ <*> _ = Nil
