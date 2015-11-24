
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module GameStatus
where

import qualified Dict as D


data GameStatus = None
                | LightOnMove
                | DarkOnMove
                | LightWin
                | DarkWin
                | Draw
    deriving (Show, Read, Eq, Bounded, Ord, Enum)


initialGameStatus :: GameStatus
initialGameStatus = LightOnMove

isGameOngoing :: GameStatus -> Bool
isGameOngoing gs = gs `elem` [LightOnMove, DarkOnMove]

isLightOnMove :: GameStatus -> Bool
isLightOnMove gs = gs == LightOnMove

isDarkOnMove :: GameStatus -> Bool
isDarkOnMove gs = gs == DarkOnMove

isGameEnded :: GameStatus -> Bool
isGameEnded gs = gs `elem` [LightWin, DarkWin, Draw]

nextGameStatus :: GameStatus -> GameStatus
nextGameStatus LightOnMove = DarkOnMove
nextGameStatus DarkOnMove = LightOnMove
nextGameStatus gs = gs

reverseOnMove :: GameStatus -> GameStatus
reverseOnMove = nextGameStatus

gameStatusString :: GameStatus -> String
gameStatusString gs =  D.findByKey gs dict_
    where dict_ = [ ( None, "None" ),
                    ( LightOnMove, "Light on move" ),
                    ( DarkOnMove, "Dark on move" ),
                    ( LightWin, "Light win" ),
                    ( DarkWin, "Dark win" ),
                    ( Draw, "Draw" )
                  ]

