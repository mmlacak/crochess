
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module ConsoleInput
where

import qualified Data.Char as DC

import qualified CommonString as CS


--separateInput :: String -> [ String ]
--separateInput str = strs_
--    where   (empty_, strs_) = C.splitAllWords str []

separateCommandAndParameters :: String -> (String, String)
separateCommandAndParameters str = (map DC.toLower cmd_, CS.stripSpace params_)
    where (cmd_, params_) = span DC.isAlphaNum $ CS.stripSpace str

