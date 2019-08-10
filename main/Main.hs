{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Main (main) where

import qualified V0
import qualified V1
import qualified V2
import qualified V3

import ConsPattern

import Control.Applicative (liftA2)
import Control.Monad.Trans.State (evalState, state)

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

pattern Nil :: V0.Vec a
pattern Nil = V0.Nil

pattern (::*) :: ConsPattern v t => a -> t a -> v a
pattern x ::* xs <- (match -> (x, xs)) where
    x ::* xs = build x xs

infixr 5 ::*

-------------------------------------------------------------------------------
-- Vectors
-------------------------------------------------------------------------------

v0 :: V0.Vec Double
v0 = pure 0.0

v1 :: V1.Vec Double
v1 = pure 1.0

v2 :: V2.Vec Double
v2 = pure 2.0

v3 :: V3.Vec Double
v3 = evalState (traverse (const $ state $ \n -> (n, succ n)) (pure ())) 0

v3a :: V3.Vec Double
v3a = 1 ::* 2 ::* 3 ::* Nil

v3b :: V3.Vec Double
v3b = 5 ::* 7 ::* 9 ::* Nil

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    print v0
    print v1
    print v2
    print $ liftA2 (*) v3 v3
    print $ liftA2 (+) v3a v3b
