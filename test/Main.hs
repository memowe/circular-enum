{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
module Main (main) where

import Data.Enum.Circular
import Data.Function
import Test.Hspec

data Direction = N | E | S | W deriving (Show, Eq, Enum, Bounded)

-- Enumeration of all members of the direction enum

allDirs :: [Direction]
allDirs = enumFrom minBound :: [Direction]

-- compare the first 2 times 'Direction'-Enum-Size items
startShouldBe :: (Show a, Eq a) => [a] -> [a] -> Expectation
startShouldBe = shouldBe `on` take (length allDirs * 2)

circularDirections :: Spec
circularDirections = describe "Circular directions" $ do

  describe "Boundaries" $ do
    it "North after West"   $ csucc W `shouldBe` N
    it "West before North"  $ cpred N `shouldBe` W

  describe "Compatible with Enum instance" $ do
    it "Successors" $
      iterate csucc minBound `startShouldBe` cycle allDirs
    it "Predecessors" $
      iterate cpred maxBound `startShouldBe` cycle (reverse allDirs)

circularNewtype :: Spec
circularNewtype = describe "Circular newtype" $ do

  describe "Boundaries" $ do
    it "North after West" 
      $ csucc (Circular W) `shouldBe` Circular N

  describe "Compatible with inner Enum instance" $ do
    it "Successors" 
      $ iterate succ minBound `startShouldBe` fmap Circular (cycle allDirs)

    it "Predecessors"
      $ iterate pred maxBound `startShouldBe` fmap Circular (cycle $ reverse allDirs)

  describe "Out of Bounds" $ do
    it "fromEnum Boundary"
      $ toEnum 4 `shouldBe` Circular N

    it "toEnum Truncation"
      $ fromEnum (toEnum 4 :: Circular Direction) `shouldBe` 0

    it "toEnum: repeating series"
      $ fmap toEnum [0..] `startShouldBe` fmap Circular (cycle allDirs)

  describe "enum[From][Then][To] circularity" $ do
    it "Stepped Enum Iteration"
      $ enumFromThen (Circular N) (Circular S) `startShouldBe` cycle (fmap Circular [N, S])

    it "enumeration Wrapping"
      $ enumFromTo (Circular S) (Circular E) `shouldBe` fmap Circular [S, W, N, E]
      -- forward iteration

    it "enumeration stepped wrapping"
      $ enumFromThenTo (Circular N) (Circular S) (Circular E) `startShouldBe` cycle (fmap Circular [N, S])
      -- produces an infinite list because 'E' can never be reached

    it "enumFrom infinity"
      $ enumFrom (Circular E) `startShouldBe` cycle (fmap Circular [E, S, W, N])

main :: IO ()
main = hspec $ do
  circularDirections
  circularNewtype
