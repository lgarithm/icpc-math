module Number where
import Data.List(findIndex)
import qualified Data.Map as Map

-- | multiply modulo m
mmm m = ((`mod` m) .) . (*)

-- | normalize mod n
nnn n = (`mod` n) . (+n) . (`mod` n)

-- | lower integer square root
sqn = truncate . sqrt . fromIntegral

-- | upper integer square root
sqm = ceiling . sqrt . fromIntegral

-- | remove all prime factors of a from n
fil n a = let d = gcd n a in  if d == 1 then n else fil (div n d) a

-- | find a solution of ax + by = d, where d = gcd(a, b)
euclid _ 0 = (1, 0)
euclid a b = let (y, x) = euclid b (mod a b) in  (x, y - (div a b) * x)

-- | find a' in [0 .. n-1] such that a * a' = d (mod n)
-- where d = gcd(a, n), where d = 1, a' is the modular inverse
associate a n = (nnn n) . fst $ euclid a n

-- | find all solutions of ax = b (mod n)
linear_eq n a b = let d = gcd a n
                  in  if mod b d /= 0 then []
                      else let n' = div n d
                               a' = associate (div a d) n'
                               x = mod ((div b d) * a') n'
                           in take d [x, x + n' .. ]

-- | find b ^ e mod n
expo _ _ 0 = 1
expo n b 1 = mod b n
expo n b e = let a = let a = expo n b (div e 2) in mmm n a a
                 in  if (mod e 2) == 1 then mmm n a b else a

-- | find the smallest number in [m .. n] that divides n
mfactor m n = head $ filter ((==0). mod n) $ [m .. sqn n] ++ [n]

-- | find all prime factors of n
pfactors n = reverse (f n 2 []) where
    f n m acc = if n == 1 || m > n then acc
                else if m * m > n then (n : acc)
                     else let p = mfactor m n in f (fil n p) (p + 1) (p:acc)

-- | euler's totient function of n
euler_totient n = foldl f n $ pfactors n where f n p = n - (div n p)

-- | find m such that a^m = 1 mod n for (a, n) = 1
order n a = let e = expo n a
                f g p = if mod g p == 0 && e (div g p) == 1 then f (div g p) p else g
                g = euler_totient n
            in  foldl f g $ pfactors g

-- | predicate for g is primitive root of p^r
ppr p r g = all f (pfactors ph) where
  n = p ^ r
  ph = n - (div n p)
  f t = expo n g (div ph t) /= 1

-- | find one primitive root of p^r
pr p r = head $ filter (ppr p r) [2 .. ]

-- | discrete logarithm of a based g modulo p^r
dlog_pr g p r a = let n = p ^ r
                      ph = n - (div n p)
                      g' = associate g n
                      m = sqm ph
                      t = Map.fromList $ zip (scanl (mmm n) (mod a n) $ replicate (m-1) (expo n g m)) [0, m ..]
                  in  let Just i = findIndex (flip Map.member t) $ scanl1 (mmm n) $ replicate m g'
                      in  let k = expo n g' i
                              Just j = Map.lookup k t
                          in  i + j

-- | find all solution of x^m = a (mod p^r)
droot_pr p r a m = let n = p ^ r
                       g = pr p r
                       ph = n - (div n p)
                   in  map (expo n g) $ linear_eq ph m $ dlog_pr g p r a

-- | find solution of x = a (mod m) = b (mod n)
crt a m b n = (`mod` (m * n)) . sum $
              map product [ [a, n, associate n m],
                            [b, m, associate m n]]
