module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (ifoldMap)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)
import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)

benchSum :: Benchmark
benchSum = mkBenchmark
  { slug: "sum"
  , title: "Finding the sum of an array"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n arbitrary
  , functions: [ benchFn "foldMap" (ala Additive foldMap :: Array Int -> Int)
               , benchFn "ifoldMap <<< const" (ala Additive (ifoldMap <<< const))
               ]
  }

main :: forall eff. Eff (BenchEffects eff) Unit
main = runSuite [benchSum]
