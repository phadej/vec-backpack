{-# LANGUAGE PatternSynonyms, ViewPatterns, MonoLocalBinds #-}
{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Lists #-}
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Main (main) where

import qualified V0
import qualified V1
import qualified V2
import qualified V3

import ConsPattern

import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Monad ((>=>))
import Control.Applicative (liftA2)
import Data.List (mapAccumL)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Vec.Lazy.Inline as GADT
import qualified Data.Vec.DataFamily.SpineStrict as DF
import qualified Data.Type.Nat as N

import GHC.Exts.Heap

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

pattern Nil :: V0.Vec a
pattern Nil = V0.Nil

pattern (:::) :: ConsPattern v t => a -> t a -> v a
pattern x ::: xs <- (match -> (x, xs))
  where x ::: xs = build x xs

infixr 5 :::

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
v3 = snd $ mapAccumL (\n _ -> (succ n, n)) 0 (pure ())

v3a :: V3.Vec Double
v3a = 1 ::: 2 ::: 3 ::: Nil

v3b :: V3.Vec Double
v3b = [5, 6, 7]

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

    putStrLn "Sizes"
    printSize "v0 :: V0 Double" v0
    printSize "v1 :: V1 Double" v1
    printSize "v2 :: V2 Double" v2
    printSize "v3 :: V3 Double" v3

    let gadt = 1.0 GADT.::: 2.0 GADT.::: 3.0 GADT.::: GADT.VNil :: GADT.Vec N.Nat3 Double
    _ <- evaluate (force gadt)
    printSize "gadt :: Vec Nat3 Double" gadt

    let df = 1.0 DF.::: 2.0 DF.::: 3.0 DF.::: DF.VNil :: DF.Vec N.Nat3 Double
    _ <- evaluate (force df)
    printSize "gadt :: Vec Nat3 Double" df

    let manual = Manual 1.0 2.0 3.0 :: Manual Double
    printSize "manual :: Manual Double" manual

  where
    printSize n x = do
        s <- calculateSize x
        putStrLn $ "| " ++ n ++ " | = " ++ show s

-------------------------------------------------------------------------------
-- Manual
-------------------------------------------------------------------------------

data Manual a = Manual !a !a !a

-------------------------------------------------------------------------------
-- calculate size
-------------------------------------------------------------------------------

calculateSize :: HasHeapRep a => a -> IO (Maybe Int)
calculateSize val = do
    c <- getClosureData val
    runMaybeT (go c)
  where
    go :: Closure -> MaybeT IO Int
    go _c@(ConstrClosure _ pargs dargs _ _ _) = do
        -- liftIO $ print _c
        ds <- traverse (liftIO . getBoxedClosureData >=> go) pargs
        return
            $ 1
            + length dargs -- data words
            + length pargs -- pointers
            + sum ds       -- size of pointed data
    go x = do
        liftIO $ print x
        MaybeT (return Nothing)
