{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Data.Enum.Circular
Description : Circular successor & predecessor for bounded enum types
Copyright   : (c) 2023-2025 Mirko Westermeier
License     : MIT

Sometimes, bounded enum types should be circular. Consider this enum
type of directions:

@
data Direction  = North
                | East
                | South
                | West
                deriving (Eq, Enum, Bounded)
@

The 'Enum' instance allows for @succ North@ to be @East@ and @succ East@
to be @South@. But in this case, one would like to have some kind of
@succ@ with @succ West = North@ again. With 'Eq' and 'Bounded' instances,
the functions defined in this module act like circular versions of 'succ'
and 'pred'.
-}

module Data.Enum.Circular (csucc, cpred, Circular(..)) where

-- | Circular version of 'succ'
csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc = unCircular . succ . Circular

-- | Circular version of 'pred'
cpred :: (Eq a, Enum a, Bounded a) => a -> a
cpred = unCircular . pred . Circular


-- | Type Alias you can use to express your intent and avoid 'Enum' functions from biting you.
--
-- Beware: this alters the behaviour of some functions, producing infinite lists (because of circularity)

newtype Circular a = Circular {unCircular :: a}
  deriving (Show, Eq, Ord)

instance (Eq a, Enum a, Bounded a) => Enum (Circular a) where
  succ :: Circular a -> Circular a
  succ (Circular x) | x == maxBound = Circular minBound
                    | otherwise     = Circular (succ x)

  pred :: Circular a -> Circular a
  pred (Circular x) | x == minBound = Circular maxBound
                    | otherwise     = Circular (pred x)

  toEnum :: Int -> Circular a
  toEnum index = let
    maxBoundIndex = fromEnum (maxBound :: a) -- relies on the fact that toEnum starts at zero
    truncatedIndex = index `mod` (maxBoundIndex + 1)
    in Circular (toEnum truncatedIndex)

  fromEnum :: Circular a -> Int
  fromEnum (Circular inner) = fromEnum inner

  enumFrom :: Circular a -> [Circular a]
  enumFrom start = cycle $ enumFromTo start (pred start)

  enumFromThen :: (Eq a, Enum a, Bounded a) => Circular a -> Circular a -> [Circular a]
  enumFromThen lower higher = let
    lowerIndex = fromEnum lower
    higherIndex = fromEnum higher
    stepSize = abs $ higherIndex - lowerIndex -- absolute step size: wraps around
    stepList i = let
      current   = toEnum i
      nextIndex = fromEnum current + stepSize
      in current : stepList nextIndex
    in stepList lowerIndex

  enumFromTo :: (Eq a, Enum a, Bounded a) => Circular a -> Circular a -> [Circular a]
  enumFromTo current target = current : if current == target
    then []
    else enumFromTo (succ current) target

  enumFromThenTo :: (Eq a, Enum a, Bounded a) => Circular a -> Circular a -> Circular a -> [Circular a]
  enumFromThenTo lower higher target = let
    lowerIndex = fromEnum lower
    higherIndex = fromEnum higher
    stepSize = abs $ higherIndex - lowerIndex -- absolute step size: wraps around
    stepListTo i = let
      current = toEnum i
      nextIndex = fromEnum current + stepSize
      in if current == target
        then []
        else current : stepListTo nextIndex
    in stepListTo lowerIndex

