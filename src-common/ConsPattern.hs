{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module ConsPattern where

class ConsPattern v t | v -> t, t -> v where
    match :: v a -> (a, t a)
    build :: a -> t a -> v a
