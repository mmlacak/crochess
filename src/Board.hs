
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module Board
where


import qualified PieceType as PT
import qualified BoardType as BT
import qualified Position as Pos

import qualified BoardInitialize as BI
import qualified BoardInitializeLight as BIL
import qualified BoardInitializeDark as BID


type Line = [ PT.PieceType ]
type Column = Line
type Row = Line
type PlayGround = [ Column ]


data Board = Board { boardType :: BT.BoardType,
                     playGround :: PlayGround }
    deriving (Show, Read, Eq)


isLineValid :: BT.BoardType -> Line -> Bool
isLineValid BT.None _ = False
isLineValid bt row = (BT.boardSize bt) == (length row)

isPlayGroundValid :: BT.BoardType -> PlayGround -> Bool
isPlayGroundValid bt pg = (length_ == size_) &&
                          (all isLineValid_ pg)
    where size_ = BT.boardSize bt
          isLineValid_ = isLineValid bt
          length_ = length pg

isBoardValid :: Board -> Bool
isBoardValid b = isPlayGroundValid (boardType b) (playGround b)


emptyColumn :: BT.BoardType -> Column
emptyColumn BT.None = []
emptyColumn bt = [ PT.None | _ <- [0 .. (BT.boardSize bt) - 1] ]

emptyPlayGround :: BT.BoardType -> PlayGround
emptyPlayGround BT.None = []
emptyPlayGround bt = [ emptyColumn bt | _ <- [0 .. (BT.boardSize bt) - 1] ]

emptyMiddleOfInitializedColumn :: BT.BoardType -> Column
emptyMiddleOfInitializedColumn BT.None = []
emptyMiddleOfInitializedColumn bt = [ PT.None | _ <- [0 .. (BT.boardSize bt) - 5] ]

initializeColumn :: BT.BoardType -> PT.PieceType -> PT.PieceType -> Column
initializeColumn BT.None _ _ = []
initializeColumn bt lightPiece darkPiece = [lightPiece] ++
                                           [PT.LightPawn] ++
                                           (emptyMiddleOfInitializedColumn bt) ++
                                           [PT.DarkPawn] ++
                                           [darkPiece]

initializeMonoliths :: BT.BoardType -> PlayGround -> PlayGround
initializeMonoliths bt pg = setPiecesInPlayGround pg pposs
    where poss = BI.initialMonolithPositions bt
          pposs = map (\ pos -> (PT.Monolith, pos)) poss

initializePlayGround :: BT.BoardType -> PlayGround
initializePlayGround BT.None = []
initializePlayGround bt = initializeMonoliths bt pg_
    where bid_ = BID.initialDarkFiguresRank bt
          bil_ = BIL.initialLightFiguresRank bt
          pg_ = [ initializeColumn bt lf_ df_ | (lf_, df_) <- zip bil_ bid_ ]

initializeBoard :: BT.BoardType -> Board
initializeBoard bt = Board { boardType=bt, playGround=initializePlayGround bt }


newBoard :: BT.BoardType -> PlayGround -> Board
newBoard bt pg = Board { boardType=bt, playGround=pg }

boardNewPlayGround :: Board -> PlayGround -> Board
boardNewPlayGround b pg | (isPlayGroundValid bt_ pg) = Board { boardType=bt_, playGround=pg }
                           | otherwise = b
    where bt_ = boardType b


isColumnIndexValid :: PlayGround -> Pos.ColumnIndex -> Bool
isColumnIndexValid pg ci = (0 <= ci) && (ci < length pg)

isRowIndexValid :: Column -> Pos.RowIndex -> Bool
isRowIndexValid f ri = (0 <= ri) && (ri < length f)

isPositionValid :: PlayGround -> Pos.Position -> Bool
isPositionValid pg (ci, ri) = (isColumnIndexValid pg ci) && (isRowIndexValid column_ ri)
    where column_ = pg !! ci

isFieldLight :: BT.BoardType -> Pos.ColumnIndex -> Pos.RowIndex -> Bool
isFieldLight bt ci ri = if BT.boardSize bt `mod` 2 == 0 then mod2_ /= 0
                        else mod2_ == 0
    where mod2_ = (ci + ri) `mod` 2

isFieldDark :: BT.BoardType -> Pos.ColumnIndex -> Pos.RowIndex -> Bool
isFieldDark bt ci ri = not $ isFieldLight bt ci ri


getField :: PlayGround -> Pos.Position -> PT.PieceType
getField pg pos@(ci, ri) | (isPositionValid pg pos) = column_ !! ri
                         | otherwise = PT.None
    where column_ = pg !! ci

setPieceInField :: Pos.PieceRowIndex -> Pos.PieceRowIndex -> PT.PieceType
setPieceInField (dest_p, dest_ri) (set_p, set_ri) = if dest_ri == set_ri then set_p else dest_p

setPieceInColumn :: Column -> Pos.PiecePosition -> Column
setPieceInColumn c pp@(p, pos@(ci, ri)) | (isRowIndexValid c ri) = new_c_
                                        | otherwise = c
    where new_c_ = [ setPieceInField c_i_ (p, ri) | c_i_ <- zip c [0 ..] ]

setPieceInPlayGround :: PlayGround -> Pos.PiecePosition -> PlayGround
setPieceInPlayGround pg pp@(p, pos@(ci, ri)) | (isPositionValid pg pos) = new_pg_
                                             | otherwise = pg
    where new_pg_ = [ if ci == i_ then setPieceInColumn c_ pp else c_ | (c_, i_) <- zip pg [0 ..] ]

setPiecesInPlayGround :: PlayGround -> Pos.PiecePositions -> PlayGround
setPiecesInPlayGround pg [] = pg
setPiecesInPlayGround pg (ppos:pposs) = setPiecesInPlayGround new_pg pposs
    where new_pg = setPieceInPlayGround pg ppos

setPieceOnBoard :: Board -> Pos.PiecePosition -> Board
setPieceOnBoard b pp = new_b_
    where pg_ = setPieceInPlayGround (playGround b) pp
          new_b_ = boardNewPlayGround b pg_

setPiecesOnBoard :: Board -> Pos.PiecePositions -> Board
setPiecesOnBoard b [] = b
setPiecesOnBoard b (pp:pps) = setPiecesOnBoard new_b_ pps
    where new_b_ = setPieceOnBoard b pp

-- movePiece2 :: Board -> LongMove -> Board
-- movePiece2 b lm@(pt, start, end) = setPieceOnBoard (setPieceOnBoard b (PT.None, start)) (pt, end)

doClearField ::  Board -> Pos.ClearField -> Board
doClearField b cf = setPieceOnBoard b (Pos.convertToPiecePosition cf)

doClearFields :: Board -> Pos.ClearFieldsList -> Board
doClearFields b cfl = setPiecesOnBoard b (Pos.convertToPiecePositions cfl)

