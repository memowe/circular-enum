{-|
Module      : Data.Enum.Circular
Description : Circular successor & predecessor for bounded enum types
Copyright   : (c) 2023-2026 Mirko Westermeier
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


--  | Type Alias you can use to express your intent and avoid 'Enum'
--    functions from biting you.
--
--    Beware: unlike regular 'Enum' types, backwards enumeration via
--    @[West, South ..]@ leads to singleton results. Since the enum is
--    circular, a "backwards" step is equivalent to a large forwards step,
--    but the enumeration ends at the "to" element earlier than expected.
--    Use 'pred' or 'cpred' explicitly with 'iterate' to go backwards.

newtype Circular a = Circular {unCircular :: a}
  deriving (Show, Eq)

instance (Eq a, Enum a, Bounded a) => Enum (Circular a) where
  succ :: Circular a -> Circular a
  succ (Circular x) | x == maxBound = Circular minBound
                    | otherwise     = Circular (succ x)

  pred :: Circular a -> Circular a
  pred (Circular x) | x == minBound = Circular maxBound
                    | otherwise     = Circular (pred x)

  toEnum :: Int -> Circular a
  toEnum = Circular . toEnum . (`mod` len)
    where len = fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1

  fromEnum :: Circular a -> Int
  fromEnum = fromEnum . unCircular

  enumFrom :: Circular a -> [Circular a]
  enumFrom = enumFromThen <*> succ

  enumFromThen :: (Eq a, Enum a, Bounded a) => Circular a -> Circular a -> [Circular a]
  enumFromThen from next = iterate goNext from
    where len     = fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1
          step    = (fromEnum next - fromEnum from) `mod` len
          goNext  = (!! step) . iterate succ

  enumFromTo :: (Eq a, Enum a, Bounded a) => Circular a -> Circular a -> [Circular a]
  enumFromTo start end = takeWhile (/= end) (enumFrom start) ++ [end]

  enumFromThenTo :: (Eq a, Enum a, Bounded a) => Circular a -> Circular a -> Circular a -> [Circular a]
  enumFromThenTo from next to
    | from == to    = [to]
    | from == next  = repeat from
    | otherwise     = from : go from (succ from)
    where go x y
            | y == goNext x &&  y == to = [to]
            | y == goNext x             = y : go y (succ y)
            |                   y == to = []
            | otherwise                 = go x (succ y)
          len     = fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1
          step    = (fromEnum next - fromEnum from) `mod` len
          goNext  = (!! step) . iterate succ
