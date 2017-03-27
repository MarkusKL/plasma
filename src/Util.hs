module Util
( (~+~)
, (~++~)
, (*-)
, (+++)
, grp
) where

infixr 5 ~+~ -- Apply triple
(~+~) :: (a -> a -> a -> b) -> (a,a,a) -> b 
f ~+~ (a,b,c) = f a b c

infixr 5 ~++~ -- Apply quadruple 
(~++~) :: (a -> a -> a -> a -> b) -> (a,a,a,a) -> b 
f ~++~ (a,b,c,d) = f a b c d

infixr 7 *- -- Scalar multiplication
(*-) :: (Num a) => a -> (a,a,a) -> (a,a,a)
a *- (a1,a2,a3) = (a*a1,a*a2,a*a3)

infixr 6 +++ -- Add triple
(+++) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a1,a2,a3) +++ (b1,b2,b3) = (a1+b1,a2+b2,a3+b3)

grp :: [a] -> [(a,a,a,a)]
grp (a:b:c:d:xs) = (a,b,c,d):(grp xs)