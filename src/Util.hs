module Util
( (~+~)
, (~++~)
, grp
) where

infixr 5 ~+~ -- Apply triple
(~+~) :: (a -> a -> a -> b) -> (a,a,a) -> b 
f ~+~ (a,b,c) = f a b c

infixr 5 ~++~ -- Apply quadruple 
(~++~) :: (a -> a -> a -> a -> b) -> (a,a,a,a) -> b 
f ~++~ (a,b,c,d) = f a b c d

grp :: [a] -> [(a,a,a,a)]
grp (a:b:c:d:xs) = (a,b,c,d):(grp xs)
