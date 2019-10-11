module Out where

import qualified In

import ConsPattern

data Vec a = (:::) !a {-# UNPACK #-} !(In.Vec a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

infixr 5 :::

instance Show a => Show (Vec a) where
    showsPrec d (x ::: xs) = showParen (d > 5)
        $ showsPrec 6 x
        . showString " ::: "
        . showsPrec 5 xs

instance Applicative Vec where
    pure x = x ::: pure x
    f ::: g <*> x ::: y = f x ::: (g <*> y)

instance ConsPattern Vec In.Vec where
    build = (:::)
    match (x ::: xs) = (x, xs)
