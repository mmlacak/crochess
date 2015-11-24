
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module CommonString
where

import qualified Data.Char as DC
import qualified Data.List as DL


dropWhileBothEnds :: (a -> Bool) -> [a] -> [a]
dropWhileBothEnds _ [] = []
dropWhileBothEnds f l = DL.dropWhileEnd f l1
    where l1 = DL.dropWhile f l

stripNotAlphaNum :: String -> String
stripNotAlphaNum str = dropWhileBothEnds (not . DC.isAlphaNum) str

stripSpace :: String -> String
stripSpace str = dropWhileBothEnds DC.isSpace str

stripSeparator :: String -> String
stripSeparator str = dropWhileBothEnds DC.isSeparator str

filterAllSeparators :: String -> String
filterAllSeparators s = filter (not . DC.isSeparator) s

filterAllSpaces :: String -> String
filterAllSpaces s = filter (not . DC.isSpace) s

--splitAllWords :: String -> [ String ] -> (String, [ String ])
--splitAllWords "" accu = ("", accu)
--splitAllWords str accu = splitAllWords new_rest_ new_accu_
--    where   (head_anum_, rest_anum_) = span DC.isAlphaNum $ stripSpace str
--            (head_punc_, rest_punc_) = span DC.isPunctuation $ stripSpace rest_anum_
--            new_rest_ = stripSpace rest_punc_
--            new_accu_ = accu ++ [ head_anum_, head_punc_ ]


