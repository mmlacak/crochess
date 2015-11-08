
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module BoardInitialize
where

import qualified BoardType as BT
import qualified Position as Pos


initialMonolithPositions :: BT.BoardType -> Pos.Positions
initialMonolithPositions bt = case bt of
                              BT.OddDiscovery -> [(1, 7), (21, 15)]
                              BT.Discovery    -> [(1, 7), (22, 16)]
                              BT.OddOne       -> [(1, 8), (23, 16)]
                              BT.One          -> [(1, 8), (24, 17)]
                              otherwise       -> []

