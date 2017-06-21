module Main where

import Prelude

import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.Monad.Eff (Eff)
import Data.Array ((..))
import Data.Const (Const(..))
import Data.Foldable (foldMap, foldr)
import Data.FoldableWithIndex (ifoldMap, ifoldr)
import Data.FunctorWithIndex (imap)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala, unwrap)
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (itraverse)
import Data.Tuple (Tuple(..), uncurry)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)

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
               , benchFn "foldr (+) 0" (foldr (+) 0)
               , benchFn "ifoldr (const (+)) 0" (ifoldr (const (+)) 0)
               , benchFn "traverse Const" $
                   unwrap <<< traverse (Const <<< Additive)
               , benchFn "itraverse (const Const)" $
                   unwrap <<< itraverse (const (Const <<< Additive))
               ]
  }

benchIndexedSum :: Benchmark
benchIndexedSum = mkBenchmark
  { slug: "indexed-sum"
  , title: "Finding \\Sum_i i * a_i of an array a"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n arbitrary
  , functions: [ benchFn "ifoldMap" $ ifoldMap (\i x -> Additive (i * x))
               , benchFn "foldMap (uncurry ...) <<< imap Tuple" $
                   foldMap (uncurry \i x -> Additive (i * x)) <<< imap Tuple
               , benchFn "ifoldr (...) 0" $
                   Additive <<< (ifoldr (\i x y -> i * x + y) 0)
               , benchFn "foldr ((uncurry (...) <<< imap Tuple) 0" $
                   Additive <<< (foldr (uncurry \i x y -> i * x + y) 0) <<<
                   imap Tuple
               , benchFn "itraverse (... Const ...)" $
                   unwrap <<< itraverse (\i x -> Const (Additive (i * x)))
               , benchFn "sequence <<< imap (... Const ...)" $
                   unwrap <<< sequence <<<
                   imap (\i x -> Const (Additive (i * x)))
               ]
  }

main :: forall eff. Eff (BenchEffects eff) Unit
main = runSuite [benchSum, benchIndexedSum]
