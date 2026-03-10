module Main (main) where

import Data.Enum.Circular
import Data.Function
import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

data Dir = N | E | S | W deriving (Show, Eq, Enum, Bounded)

-- Enumeration of all members of the direction enum
allDirs :: [Dir]
allDirs = enumFrom minBound :: [Dir]

instance Arbitrary Dir where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Circular a) where
  arbitrary = Circular <$> arbitrary

-- compare the first 2 times 'Dir'-Enum-Size items
startShouldBe :: (Show a, Eq a) => [a] -> [a] -> Expectation
startShouldBe = shouldBe `on` take (length allDirs * 2)

main :: IO ()
main = hspec $ do

  describe "Circular directions" $ do

    describe "Boundaries" $ do
      it "North after West"   $ csucc W `shouldBe` N
      it "West before North"  $ cpred N `shouldBe` W

    describe "Compatible with Enum instance" $ do
      it "Successors" $
        iterate csucc minBound `startShouldBe` cycle allDirs
      it "Predecessors" $
        iterate cpred maxBound `startShouldBe` cycle (reverse allDirs)

  describe "Circular newtype" $ do

    describe "Boundaries" $ do
      it "North after West"   $ succ (Circular W) `shouldBe` Circular N
      it "West before North"  $ pred (Circular N) `shouldBe` Circular W

    describe "Reversibility" $ do
      prop "succ o pred = id" $ \dir ->
        (succ . pred) (Circular dir) `shouldBe` Circular (dir :: Dir)
      prop "pred o succ = id" $ \dir ->
        (pred . succ) (Circular dir) `shouldBe` Circular (dir :: Dir)

    describe "Compatible with inner Enum instance" $ do
      prop "Successors" $ \dir -> dir /= (maxBound :: Dir) ==>
        succ (Circular dir) `shouldBe` Circular (succ dir)
      prop "Predecessors" $ \dir -> dir /= (minBound :: Dir) ==>
        pred (Circular dir) `shouldBe` Circular (pred dir)

    describe "to/fromEnum" $ do
      let len = length allDirs
      it "Upper example" $
        toEnum 4 `shouldBe` Circular N
      it "Lower example" $
        toEnum (-1) `shouldBe` Circular W
      prop "Cycle down" $ \n -> n >= fromEnum (maxBound :: Dir) ==>
        toEnum n `shouldBe` (toEnum (n `mod` len) :: Circular Dir)
      prop "Cycle up" $ \n -> n < 0 ==>
        toEnum n `shouldBe` (toEnum (n `mod` len) :: Circular Dir)
      prop "toEnum Truncation" $ \n ->
        fromEnum (toEnum n :: Circular Dir) `shouldBe` n `mod` len
      prop "toEnum: repeating series" $ \n -> n >= 0 ==>
        let toEnums   = toEnum <$> [0..]
            cAllDirs  = Circular <$> cycle allDirs
        in  toEnums !! n `shouldBe` cAllDirs !! n
      prop "Roundtrip toEnum . fromEnum" $ \cd ->
        toEnum (fromEnum cd) `shouldBe` (cd :: Circular Dir)
      prop "'Roundtrip' fromEnum . toEnum . fromEnum" $ \cd ->
        let i = fromEnum (cd :: Circular Dir)
        in  fromEnum (toEnum i :: Circular Dir) `shouldBe` i

    describe "enum[From][Then][To] circularity" $ do

      describe "Examples" $ do
        it "Stepped Enum Iteration" $
          enumFromThen (Circular N) (Circular S)
            `startShouldBe` cycle (fmap Circular [N, S])
        it "enumeration Wrapping" $
          enumFromTo (Circular S) (Circular E)
            `shouldBe` fmap Circular [S, W, N, E]
          -- forward iteration
        it "enumeration stepped wrapping" $
          enumFromThenTo (Circular N) (Circular S) (Circular E)
            `shouldBe` [Circular N]
          -- consistent with [1,3..2]
        it "enumFrom infinity" $
          enumFrom (Circular E)
            `startShouldBe` cycle (fmap Circular [E, S, W, N])

      describe "General properties" $ do

        describe "enumFromThen" $ do
          prop "Correct start of enumFromThen: first" $ \cd cd' ->
            head (enumFromThen cd cd') `shouldBe` (cd :: Circular Dir)
          prop "Correct start of enumFromThen: second" $ \cd cd' ->
            enumFromThen cd cd' !! 1 `shouldBe` (cd' :: Circular Dir)
          prop "Start = next -> infinite" $ \cd ->
            enumFromThen cd cd `startShouldBe` repeat (cd :: Circular Dir)
          prop "Correct step size of enumFromThen" $ \cd n -> n /= 0 ==>
            let cd' = toEnum (fromEnum (cd :: Circular Dir) + n)
                ds  = enumFromThen cd cd'
                shouldBeModLen = shouldBe `on` (`mod` length allDirs)
            in  forAll (sized $ \s -> choose (0, s)) $ \i ->
                  let cd1 = ds !! i
                      cd2 = ds !! (i+1)
                  in  (fromEnum cd2 - fromEnum cd1) `shouldBeModLen` n

        describe "enumFrom: enumFromThen with succ" $ do
          prop "Same list starts" $ \cd ->
            enumFrom cd
              `startShouldBe` enumFromThen (cd :: Circular Dir) (succ cd)

        describe "enumFromThenTo: finite enumFromThen" $ do
          prop "Start = end -> singleton" $ \cd cd' -> cd' /= cd ==>
            enumFromThenTo cd cd' cd `shouldBe` [cd :: Circular Dir]
          prop "Start = next -> infinite" $ \cd target -> cd /= target ==>
            enumFromThenTo cd cd target
              `startShouldBe` repeat (cd :: Circular Dir)
          prop "Prefix of enumFromThen" $ \cd cd' target -> cd /= cd' ==>
            enumFromThenTo cd cd' target
              `isPrefixOf` enumFromThen cd (cd' :: Circular Dir)

        describe "enumFromTo: enumFromThenTo with succ" $ do
          prop "Same list" $ \cd target ->
            enumFromTo cd target
              `shouldBe` enumFromThenTo cd (succ cd :: Circular Dir) target
