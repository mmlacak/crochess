
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module ParseMove
where

import qualified Text.Parsec as TP
import qualified Text.Parsec.Char as TPC
import qualified Text.Parsec.String as TPS
import Text.Parsec ( (<|>), (<?>) )

import qualified Common as C
import qualified BoardType as BT
import qualified Board as B
import qualified Position as Pos
import qualified ParsePosition as PPos
import qualified ParsePiece as PP
import qualified Move as M
import qualified GameStatus as GS
import qualified Rules as R


move :: TP.Parsec String (C.LazyBox R.Rules) M.Move
move = do
    pls <- plies
    return M.Move { M.plies=pls,
                    M.condition=M.NoCondition } -- TODO :: handle condition

plies :: TP.Parsec String (C.LazyBox R.Rules) [M.Ply]
plies = TP.sepBy1 ply (TPC.char '~')

ply :: TP.Parsec String (C.LazyBox R.Rules) M.Ply
ply = do
    TP.optional (TPC.char '[')
    piece <- TP.option 'P' TPC.upper
    (startPos, endPos) <- (TP.try plyLongAlg) <|> 
                          (TP.try plyShortAlgDisambiguingCol) <|> 
                          (TP.try plyShortAlgDisambiguingRow) <|> 
                          plyShortAlg
    TP.optional (TPC.char ']')

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let ply_ = M.Ply { M.piece=piece_,
                       M.start=startPos,
                       M.end=endPos,
                       M.sideEffect=M.dummySideEffect } -- TODO :: handle sideEffect
    return ply_

plyShortAlg :: TP.Parsec String (C.LazyBox R.Rules) (Pos.Position, Pos.Position)
plyShortAlg = do
    endCol <- TPC.lower
    endRow <- TP.many1 TPC.digit

    lb <- TP.getState
    return (Pos.dummyPosition, (position endCol endRow lb))

plyShortAlgDisambiguingCol :: TP.Parsec String (C.LazyBox R.Rules) (Pos.Position, Pos.Position)
plyShortAlgDisambiguingCol = do
    startCol <- TPC.lower
    endCol <- TPC.lower
    endRow <- TP.many1 TPC.digit

    lb <- TP.getState
    return (((columnIndex startCol lb), Pos.dummyRowIndex), (position endCol endRow lb))

plyShortAlgDisambiguingRow :: TP.Parsec String (C.LazyBox R.Rules) (Pos.Position, Pos.Position)
plyShortAlgDisambiguingRow = do
    startRow <- TP.many1 TPC.digit
    endCol <- TPC.lower
    endRow <- TP.many1 TPC.digit

    lb <- TP.getState
    return ((Pos.dummyColumnIndex, (rowIndex startRow lb)), (position endCol endRow lb))

plyLongAlg :: TP.Parsec String (C.LazyBox R.Rules) (Pos.Position, Pos.Position)
plyLongAlg = do
    startCol <- TPC.lower
    startRow <- TP.many1 TPC.digit
    TP.optional (TPC.char '-')
    endCol <- TPC.lower
    endRow <- TP.many1 TPC.digit

    lb <- TP.getState
    return ((position startCol startRow lb), (position endCol endRow lb))

columnIndex :: Char -> C.LazyBox R.Rules -> Pos.ColumnIndex
columnIndex col (C.LazyBox r) = PPos.columnIndexInt col bt 
    where b = R.board r
          bt = B.boardType b

rowIndex :: String -> C.LazyBox R.Rules -> Pos.RowIndex
rowIndex row (C.LazyBox r) = (read row :: Pos.RowIndex) - 1

position :: Char -> String -> C.LazyBox R.Rules -> Pos.Position
position col row lb = ((columnIndex col lb), (rowIndex row lb))

parseMove :: String -> R.Rules -> M.Move
parseMove s r = move_
    where m_ = TP.runParser move lb_ "fail" s
          lb_ = C.LazyBox r
          move_ = case m_ of Right move -> move
                             Left errorMsg -> M.dummyMove

-- parseMove :: String -> Either TP.ParseError M.Move
-- parseMove s = TP.parse move "fail" s

