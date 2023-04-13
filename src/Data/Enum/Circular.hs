{-|
Module      : Data.Enum.Circular
Description : Circular successor & predecessor for bounded enum types
Copyright   : (c) 2023 Mirko Westermeier
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

module Data.Enum.Circular (csucc, cpred) where

-- | Circular version of 'succ'
csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc x | x == maxBound = minBound
        | otherwise     = succ x

-- | Circular version of 'pred'
cpred :: (Eq a, Enum a, Bounded a) => a -> a
cpred x | x == minBound = maxBound
        | otherwise     = pred x
