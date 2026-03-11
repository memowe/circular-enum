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
--
--    Also assumes that the underlying 'Enum' instance is well-behaved,
--    i.e. @fromEnum minBound == 0@. This holds for all types using
--    @deriving Enum@.

newtype Circular a = Circular {unCircular :: a}
  deriving (Show, Eq)

instance (Eq a, Enum a, Bounded a) => Enum (Circular a) where

  toEnum :: Int -> Circular a
  toEnum = Circular . toEnum . (`mod` len)
    where len = fromEnum (maxBound :: a) + 1

  fromEnum :: Circular a -> Int
  fromEnum = fromEnum . unCircular

  enumFromTo :: Circular a -> Circular a -> [Circular a]
  enumFromTo start end = map toEnum $ enumFromTo i (i + dist)
    where i     = fromEnum start
          j     = fromEnum end
          dist  = (j - i) `mod` len
          len   = fromEnum (maxBound :: a) + 1

  enumFromThenTo :: Circular a -> Circular a -> Circular a -> [Circular a]
  enumFromThenTo from next to
    | step == 0 = repeat from
    | otherwise = map toEnum $ enumFromThenTo i (i + step) (i + dist)
    where i     = fromEnum from
          j     = fromEnum next
          k     = fromEnum to
          step  = (j - i) `mod` len
          dist  = (k - i) `mod` len
          len   = fromEnum (maxBound :: a) + 1
