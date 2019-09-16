{-# LANGUAGE DeriveTraversable #-}
module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.State (runState, state)
import Criterion.Main (defaultMain, bench, whnf)
import Data.Foldable (foldl')
import Data.List (unfoldr)
import System.Random.SplitMix (SMGen, mkSMGen, nextDouble)

import qualified V3

import qualified Data.Vec.Lazy.Inline as GADT
import qualified Data.Vec.DataFamily.SpineStrict as DF
import qualified Data.Type.Nat as N

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

size :: Int
size = 100

-------------------------------------------------------------------------------
-- backpack
-------------------------------------------------------------------------------

dataV3 :: [V3.Vec Double]
dataV3 = take size $ unfoldr (Just . f) (mkSMGen 42) where
    f :: SMGen -> (V3.Vec Double, SMGen)
    f = runState (traverse (const $ state nextDouble) (pure ()))

benchV3 :: [V3.Vec Double] -> Double
benchV3 = foldl' (*) 1.0 . foldl' (liftA2 (+)) (pure 0.0)

-------------------------------------------------------------------------------
-- Vec
-------------------------------------------------------------------------------

type GADT = GADT.Vec N.Nat3

dataGADT :: [GADT Double]
dataGADT = take size $ unfoldr (Just . f) (mkSMGen 42) where
    f :: SMGen -> (GADT Double, SMGen)
    f = runState (traverse (const $ state nextDouble) (pure ()))

benchGADT :: [GADT Double] -> Double
benchGADT = GADT.foldr (*) 1.0 . foldl' (GADT.zipWith (+)) (pure 0.0)

-------------------------------------------------------------------------------
-- Vec DF
-------------------------------------------------------------------------------

type DF = DF.Vec N.Nat3

dataDF :: [DF Double]
dataDF = take size $ unfoldr (Just . f) (mkSMGen 42) where
    f :: SMGen -> (DF Double, SMGen)
    f = runState (traverse (const $ state nextDouble) (pure ()))

benchDF :: [DF Double] -> Double
benchDF = DF.foldr (*) 1.0 . foldl' (DF.zipWith (+)) (pure 0.0)

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

type List = []

dataList :: [List Double]
dataList = take size $ unfoldr (Just . f) (mkSMGen 42) where
    f :: SMGen -> (List Double, SMGen)
    f = runState (traverse (const $ state nextDouble) [(), (), ()])

benchList :: [List Double] -> Double
benchList = foldl' (*) 1.0 . foldl' (zipWith (+)) [0, 0, 0]

-------------------------------------------------------------------------------
-- Manual
-------------------------------------------------------------------------------

data Manual a = Manual !a !a !a
  deriving (Functor, Foldable, Traversable)

dataManual :: [Manual Double]
dataManual = take size $ unfoldr (Just . f) (mkSMGen 42) where
    f :: SMGen -> (Manual Double, SMGen)
    f = runState (traverse (const $ state nextDouble) (pure ()))

benchManual :: [Manual Double] -> Double
benchManual = foldl' (*) 1.0 . foldl' (liftA2 (+)) (pure 0.0)

instance Applicative Manual where
    pure x = Manual x x x
    liftA2 f (Manual x1 x2 x3) (Manual y1 y2 y3) = Manual (f x1 y1) (f x2 y2) (f x3 y3)

-------------------------------------------------------------------------------
-- Bench
-------------------------------------------------------------------------------

main :: IO ()
main = do
    print $ benchV3   dataV3
    print $ benchList dataList
    print $ benchGADT dataGADT
    print $ benchDF   dataDF
    print $ benchManual  dataManual
    defaultMain
        [ bench "Backpack" $ whnf benchV3   dataV3
        , bench "List"     $ whnf benchList dataList
        , bench "GADT"     $ whnf benchGADT dataGADT
        , bench "DF"       $ whnf benchDF   dataDF
        , bench "Manual"   $ whnf benchManual  dataManual
        ]
