module Main (main) where

import Data.Enum.Circular
import Data.Function
import Test.Hspec

data Direction = N | E | S | W deriving (Show, Eq, Enum, Bounded)

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

  where allDirs         = enumFrom minBound :: [Direction]
        startShouldBe   = shouldBe `on` take (length allDirs * 2)

main :: IO ()
main = hspec circularDirections
