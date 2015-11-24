
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module Dict
where

import qualified Data.Char as DC


type Dict key value = [(key, value)]
type StringDict value = Dict String value

findByKey :: (Eq key) => key -> Dict key value -> value
findByKey key dict = snd . head . filter (\ (k, v) -> key == k) $ dict

findByLowerCaseKey :: String -> StringDict value -> value
findByLowerCaseKey key dict = snd . head . filter (\ (k, v) -> key_ == (map DC.toLower k) ) $ dict
    where key_ = map DC.toLower key

isKeyInDict :: (Eq key) => key -> Dict key value -> Bool
isKeyInDict key dict = any (\ (k, v) -> k == key) dict

isLowerCaseKeyInDict :: String -> StringDict value -> Bool
isLowerCaseKeyInDict key dict = any (\ (k, v) -> (map DC.toLower k) == key_) dict
    where key_ = map DC.toLower key


