x :: Integer
x = 99
-- to :reload

y :: Integer
y = x + 10

f :: Integer -> Integer
f a = a * 55

g :: Integer -> Integer
g = \a -> a * 55

h :: Integer -> Integer -> Integer
h = \q r -> (q + r) * 2

i :: Integer -> Integer -> Integer
i = \q -> \r -> (q + r) * 2

j :: Integer -> Integer -> Integer
j q r = (q + r) * 2


(.+.) :: Integer -> Integer -> Integer
(.+.) q r = (q + r) * 2

usej1 = j 4 5
usej2 = 4 `j` 5
usej3 = (.+.) 4 5
usej4 = 4 .+. 5

k :: (Integer -> Integer) -> Integer
k z = z 88
-- k (\x -> x + 1)
-- k f

--m v = v
-- m = \v -> v
--m :: p -> p

-- data Name {0.. type vars } = {constructor0 {0...args} |  {constructor1 {0...args} | ...

data Boolean = Troo | Falsy
  deriving Show

(.&&.) :: Boolean -> Boolean -> Boolean
(.&&.) = \p -> \q -> case p of
  Falsy -> Falsy
  Troo -> q

bottom :: Boolean
bottom = bottom

blah = Falsy .&&. bottom

--(.&&.) = \p -> \q -> case p of
--  Falsy -> Falsy
--  Troo -> case q of
--    Falsy -> Falsy
--    Troo -> Troo

o :: p -> q -> p
o = \a b -> a

data Hi a = High a | Low
  deriving Show

r :: a -> Hi a -> a
r = \ey h -> case h of
  High w -> w
  Low -> ey

mapHi :: (a -> b) -> Hi a -> Hi b
mapHi     f          Low    = Low
mapHi     f          (High a) = High (f a)

mapHii :: (a -> b) -> Hi a -> Hi b
mapHii = \f -> \hi -> case hi of
  Low -> Low
  High a -> High (f a)

pie = 3

data Shape =
  Rectangle Integer Integer
  | Circle Integer
  | Triangle Integer Integer Integer
  deriving (Show, Eq)

class Equal a where
  (===) :: a -> a -> Bool

instance Equal Shape where
  (===) (Rectangle w1 h1) (Rectangle w2 h2) =
    (w1 == w2) && (h1 == h2)
  (===) (Circle r1) (Circle r2) =
    r1 == r2
  (===) (Triangle a1 b1 c1) (Triangle a2 b2 c2) =
    (a1 == a2) && (b1 == b2) && (c1 == c2)
  (===) _ _ =
    False

perimeter :: Shape -> Integer
perimeter (Rectangle w h) = (w + h) * 2
perimeter (Circle r) = r * 2 * pie
perimeter (Triangle a b c) = a + b + c


