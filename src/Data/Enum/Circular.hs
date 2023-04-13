module Data.Enum.Circular (csucc, cpred) where

csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc x | x == maxBound = minBound
        | otherwise     = succ x

cpred :: (Eq a, Enum a, Bounded a) => a -> a
cpred x | x == minBound = maxBound
        | otherwise     = pred x
