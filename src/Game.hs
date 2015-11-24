
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module Game
where

import qualified BoardType as BT
import qualified Rules as R


data Game = Game { rules :: R.Rules }
-- TODO :: add aI :: (Maybe ?) Ai
    deriving (Show, Read)


initializeGame :: BT.BoardType -> Game
initializeGame bt = Game { rules=(R.initializeRules bt) }

