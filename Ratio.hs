module Ratio(
  Ratio(..)
) where
  import qualified Data.Ratio as R
  data Ratio a = Ratio a a

  instance (RealFrac a) => Num (Ratio a) where
    (Ratio a b) * (Ratio c d) = reduceRatio $ Ratio (a*c) (b*d)
    (+) = addR
    abs (Ratio a b) = reduceRatio $ Ratio (abs a) (abs b)
    signum (Ratio a b) = reduceRatio $ Ratio ((signum a) * (signum b)) 1
    fromInteger a = reduceRatio $ Ratio (fromInteger a) 1
    negate (Ratio a b) = reduceRatio $ Ratio (negate a) b

  instance (RealFrac a) => Fractional (Ratio a) where
    recip (Ratio a b) = reduceRatio $ Ratio b a
    fromRational a = reduceRatio $ Ratio n d
      where n = fromInteger $ R.numerator a
            d = fromInteger $ R.denominator a

  instance (RealFrac a, Show a) => Show (Ratio a) where
    show (Ratio a b)
      | fromIntegral a' == a && fromIntegral b' == b = show' a' b'
      | show b == "1.0" = show a
      | otherwise = (show a)++"/"++(show b)
      where a' = truncate a
            b' = truncate b
            show' x y
              | y == 1 = show x
              | otherwise = (show x)++"/"++(show y)

  instance (Num a, Eq a) => Eq (Ratio a) where
    (Ratio a b) == (Ratio c d) = a*d == b*c

  instance (Num a, Ord a) => Ord (Ratio a) where
    (Ratio a b) <= (Ratio c d) = a*d <= b*c

  instance (RealFrac a, Fractional a) => Real (Ratio a) where
    toRational (Ratio a b) = toRational (a/b)

  addR :: (Num a, Eq a) => Ratio a -> Ratio a -> Ratio a
  addR (Ratio a b) (Ratio c d)
    | b == d = Ratio (a+c) b
    | otherwise = Ratio (a*d+b*c) (b*d)

  gcd' :: (RealFrac a, Integral b) => a -> a -> b
  gcd' a b
    | fromIntegral a' == a && fromIntegral b' == b = gcd a' b'
    | otherwise = 1
    where a' = truncate a
          b' = truncate b

  reduceRatio :: (RealFrac a) => Ratio a -> Ratio a
  reduceRatio (Ratio a b) = Ratio (a/g) (b/g)
    where g = fromIntegral $ gcd' a b
