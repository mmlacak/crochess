
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module Common
where


newtype LazyBox a = LazyBox a
    deriving (Show, Read) -- , Eq

unwrapLazyBox :: LazyBox a -> a
unwrapLazyBox (LazyBox x) = x


infixr 5 <:> -- Same as : .
( <:> ) :: Maybe a -> [a] -> [a]
Just a <:> l = a : l
Nothing <:> l = l


firstIndex :: Eq a => a -> [a] -> Maybe Int
firstIndex _ [] = Nothing 
firstIndex x l = firstIndexOffset x l 0 

firstIndexOffset :: Eq a => a -> [a] -> Int -> Maybe Int
firstIndexOffset _ [] _ = Nothing
firstIndexOffset x (y:ys) offset = if x == y then Just offset
                                             else firstIndexOffset x ys (offset + 1)

numberedIndex :: Eq a => a -> [a] -> Int -> Maybe Int
numberedIndex _ [] _ = Nothing
numberedIndex x l stepOver = numberedIndexOffset x l stepOver 0

numberedIndexOffset :: Eq a => a -> [a] -> Int -> Int -> Maybe Int
numberedIndexOffset _ [] _ _ = Nothing
numberedIndexOffset x (y:ys) stepOver offset = 
    if x == y then 
        if stepOver == 0 then Just offset
                         else numberedIndexOffset x ys (stepOver - 1) (offset + 1)
              else numberedIndexOffset x ys stepOver (offset + 1)

