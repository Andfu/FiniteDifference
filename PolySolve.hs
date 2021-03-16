import Data.List(genericLength, genericIndex, reverse, inits)
import Ratio

--Main methods: polynomial and solve
--polynomial returns a polynomial function constructed using the coefficients
--solve finds the coefficients using a list of initial values.
--  The first value is assumed to be x=0, and the next x=1, x=2...

polynomial :: [Ratio Rational] -> (Ratio Rational -> Ratio Rational)
polynomial coeffs = sumFuncs (take l $ helper 1 coeffs)
  where helper n (c:cs) = [\x->x^(l-n)*c]++(helper (n+1) cs)
        l = genericLength coeffs
        sumFuncs fs y = sum $ map (\x->x y) fs

solve :: [Ratio Rational] -> [Ratio Rational]
solve = getCoeffs . genD

getCoeffs :: (RealFrac a, Eq a) => [Ratio a] -> [Ratio a]
getCoeffs d = take (genericLength d) $ helper d 0
  where helper xs n =
          let coeff = getCoeff n xs
          in coeff : helper (normalizeD coeff xs) (n+1)

getCoeff :: (RealFrac a, Eq a) => Int -> [Ratio a] -> Ratio a
getCoeff x d
  | n /= 0 = const / coeff
  | otherwise = 0
  where const = genericIndex d (n-1)
        coeff = flip genericIndex (n-1) $ map (fromIntegral.last) $ genL l (l-n+1)
        l = genericLength d
        n = genericLength d - x

normalizeD :: (RealFrac a, Eq a) => Ratio a -> [Ratio a] -> [Ratio a]
normalizeD c d = sumVect d $ scale (-c) col
  where col = map (fromIntegral.last) $ genL l (l-n+1)
        l = genericLength d
        n = genericLength . dropWhile (\x->x==0) $ reverse d

genD :: (Num a, Eq a) => [a] -> [a]
genD i = map head $ ds [i]
  where ds l
          | isHomogenous $ last l = l
          | otherwise = ds $ l ++ [diff $ last l]

diff :: (Num a) => [a] -> [a]
diff xs = map diff' [1..genericLength xs - 1]
  where diff' n = (genericIndex xs n) - (genericIndex xs (n-1))

isHomogenous :: (Eq a) => [a] -> Bool
isHomogenous [] = True
isHomogenous (x:[]) = True
isHomogenous (x:y:xs) = x==y && isHomogenous (y:xs)

genL :: (Integral a) => a -> a -> [[a]]
genL m c = (flip genericIndex $ (m-1)) $ iterate (appendRow c) [first]
  where first = ((replicate (fromIntegral $ c-1) 0)++) $ (1:) $ replicate (fromIntegral $ m-c) 0

appendRow :: (Integral a) => a -> [[a]] -> [[a]]
appendRow c l = l ++ [sumVectors $ scaleVectors coeffs expands]
  where coeffs = init $ last l
        expands = map (expand m) [1..m-1]
        m = genericLength (l !! 0)

sumVectors :: (Num a) => [[a]] -> [a]
sumVectors (v:[]) = v
sumVectors (v:vs) = sumVect v $ sumVectors vs

sumVect :: (Num a) => [a] -> [a] -> [a]
sumVect = zipWith (+)

scaleVectors :: (Num a) => [a] -> [[a]] -> [[a]]
scaleVectors (s:[]) (v:[]) = (scale s v):[]
scaleVectors (s:ss) (v:vs) = (scale s v):(scaleVectors ss vs)

scale :: (Num a) => a -> [a] -> [a]
scale s v = map (s*) v

expand :: (Integral a) => a -> a -> [a]
expand m n = map (genTerm m n) [0, 1..m-1]

genTerm :: (Integral a) => a -> a -> a -> a
genTerm m i k
  | i <= k && k <= m-1 = choose (m-i) (k-i+1)
  | otherwise = 0

choose :: (Integral a) => a -> a -> a
choose n k
  | k > n || k < 0 = 0
  | otherwise = (fact n) `div` ((fact k) * (fact (n-k)))

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = (n*) $ fact (n-1)
