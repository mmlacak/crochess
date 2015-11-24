
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module Rules
where


import qualified BoardType as BT
import qualified BoardInitializeLight as BIL
import qualified BoardInitializeDark as BID

import qualified Board as B
import qualified Position as Pos
import qualified Move as Move
import qualified GameStatus as GS


type CastlingFigureMovement = (Pos.PiecePosition, Bool)
type CastlingFigureMovementList = [ CastlingFigureMovement ]

dummyCastlingFigureMovement :: CastlingFigureMovement
dummyCastlingFigureMovement = (Pos.dummyPiecePosition, False)


data Rules = Rules { board :: B.Board,
                     gameStatus :: GS.GameStatus,  
                     moveList :: Move.MoveList,
                     hasLightKingMoved :: Bool,
                     hasDarkKingMoved :: Bool,
                     hasCastlingFiguresMoved :: CastlingFigureMovementList,
                     enPassantColumn :: Pos.ColumnIndex }
    deriving (Show, Read)

initializeRules :: BT.BoardType -> Rules
initializeRules bt = Rules { board=(B.initializeBoard bt), 
                             gameStatus=GS.initialGameStatus,
                             moveList=[], 
                             hasLightKingMoved=False,
                             hasDarkKingMoved=False,
                             hasCastlingFiguresMoved=new_hcfm_,
                             enPassantColumn=Pos.dummyColumnIndex }
    where lcfm_ = BIL.lightCastlingFiguresInitialPositions bt
          dcfm_ = BID.darkCastlingFiguresInitialPositions bt
          cfm_ = lcfm_ ++ dcfm_
          new_hcfm_ = [ (_pp, False) | _pp <- cfm_ ]


appendCastlingFigureMovement :: Rules -> Pos.PiecePosition -> Rules
appendCastlingFigureMovement r@(Rules _ _ _ _ _ hcfm _) pp = r { hasCastlingFiguresMoved=new_hcfm_ }
    where new_hcfm_ = hcfm ++ [ (pp, False) ]

removeCastlingFigureMovementFromList :: CastlingFigureMovementList -> Pos.PiecePosition -> CastlingFigureMovementList
removeCastlingFigureMovementFromList [] _ = []
removeCastlingFigureMovementFromList cfml@(cfm:cfms) pp = if found_ then removeCastlingFigureMovementFromList cfms pp
                                                                    else cfm : removeCastlingFigureMovementFromList cfms pp
    where found_ = pp == fst cfm

removeCastlingFigureMovement :: Rules -> Pos.PiecePosition -> Rules
removeCastlingFigureMovement r@(Rules _ _ _ _ _ hcfm _) pp = r { hasCastlingFiguresMoved=new_hcfm_ }
    where new_hcfm_ = removeCastlingFigureMovementFromList hcfm pp

updateCastlingFigureMovementFromList :: CastlingFigureMovementList -> Pos.PiecePosition -> Bool -> CastlingFigureMovementList
updateCastlingFigureMovementFromList [] _ _ = []
updateCastlingFigureMovementFromList cfml@(cfm:cfms) pp b = if found_ then updateCastlingFigureMovementFromList cfms pp b
                                                                      else new_cfm_ : updateCastlingFigureMovementFromList cfms pp b
    where found_ = pp == fst cfm
          new_cfm_ = (pp, b)

updateCastlingFigureMovement :: Rules -> Pos.PiecePosition -> Bool -> Rules
updateCastlingFigureMovement r@(Rules _ _ _ _ _ hcfm _) pp b = r { hasCastlingFiguresMoved=new_hcfm_ }
    where new_hcfm_ = removeCastlingFigureMovementFromList hcfm pp

