
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module ParseMove
where

import qualified Text.Parsec as TP
import qualified Text.Parsec.Char as TPC
import qualified Text.Parsec.String as TPS
import Text.Parsec ( (<|>), (<?>) )

import qualified Common as C
import qualified PieceType as PT
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
    cond <- condition
    return M.Move { M.plies=pls,
                    M.condition=cond }

plies :: TP.Parsec String (C.LazyBox R.Rules) [M.Ply]
plies = TP.sepBy1 ply (TPC.char '~')


-- Ply

ply :: TP.Parsec String (C.LazyBox R.Rules) M.Ply
ply = do
    TP.optional (TPC.char '[')
    piece <- TP.option 'P' TPC.upper
    (startPos, endPos) <- (TP.try plyLongAlg) <|>
                          (TP.try plyShortAlgDisambiguingCol) <|>
                          (TP.try plyShortAlgDisambiguingRow) <|>
                          plyShortAlg
    se <- (TP.try sideEffect)
    TP.optional (TPC.char ']')

    lb <- TP.getState
    -- let rules_ = C.unwrapLazyBox lb
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb -- rules_
    -- let board_ = R.board rules_ -- $ C.unwrapLazyBox lb
    -- let new_gs_ = GS.nextGameStatus gameStatus_
    -- let new_rules_ = rules_ { R.gameStatus = new_gs_ }
    -- TP.putState $ C.LazyBox new_rules_

    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- PP.pieceByConsoleSymbol piece
    let ply_ = M.Ply { M.piece=piece_,
                       M.start=startPos,
                       M.end=endPos,
                       M.sideEffect=se }
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


-- Condition

condition :: TP.Parsec String (C.LazyBox R.Rules) M.ConditionType
condition = do
    cond <- (TP.try condDoubleCheck) <|>
            (TP.try condCheckMate) <|>
            (TP.try condCheck) <|>
            condNoCondition
    return cond

condCheck :: TP.Parsec String (C.LazyBox R.Rules) M.ConditionType
condCheck = do
    _dc <- (TPC.char '+')
    return M.Check

condDoubleCheck :: TP.Parsec String (C.LazyBox R.Rules) M.ConditionType
condDoubleCheck = do
    _dc <- (TPC.char '+')
    _dc2 <- (TPC.char '+')
    return M.DoubleCheck

condCheckMate :: TP.Parsec String (C.LazyBox R.Rules) M.ConditionType
condCheckMate = do
    _dc <- (TPC.char '#')
    return M.CheckMate

condNoCondition :: TP.Parsec String (C.LazyBox R.Rules) M.ConditionType
condNoCondition = return M.NoCondition


-- Side effect

sideEffect :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
sideEffect = do
    se_ <- (TP.try sePromotion) <|>
           (TP.try seConversion) <|>
           (TP.try seCapture) <|>
           (TP.try seEnPassant) <|>
           (TP.try seTeleportationOwn) <|>
           (TP.try seTeleportationOpponent) <|>
           (TP.try seKingSidedCastling) <|>
           (TP.try seQueenSidedCastling) <|>
           (TP.try seSummoningOwn) <|>
           (TP.try seSummoningOpponent) <|>
           seNoSideEffect
    return se_

seNoSideEffect :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seNoSideEffect = return M.dummySideEffect

sePromotion :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
sePromotion = do
    _se <- (TPC.char '=')
    piece <- TP.option ' ' TPC.upper

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.Promotion,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_

seConversion :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seConversion = do
    _se <- (TPC.char '%')
    piece <- TP.option ' ' TPC.upper

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.Conversion,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_

seCapture :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seCapture = do
    _se <- (TPC.char '/')
    piece <- TP.option ' ' TPC.upper

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.opponentPieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.Capture,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_

seEnPassant :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seEnPassant = do
    _se <- (TPC.char ':')
    piece <- TP.option ' ' TPC.upper

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.opponentPieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.EnPassant,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_

seTeleportationOwn :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seTeleportationOwn = do
    _se <- (TPC.char '*')
    piece <- TP.option ' ' TPC.upper
    endCol <- TPC.lower
    endRow <- TP.many1 TPC.digit

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let pos_ = position endCol endRow lb
    let se_ = M.SideEffect { M.seType=M.TeleportationOwn,
                             M.sePiece=piece_,
                             M.sePosition=pos_ }
    return se_

seTeleportationOpponent :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seTeleportationOpponent = do
    _se <- (TPC.char '/')
    piece <- TP.option ' ' TPC.upper
    endCol <- TPC.lower
    endRow <- TP.many1 TPC.digit

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.opponentPieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let pos_ = position endCol endRow lb
    let se_ = M.SideEffect { M.seType=M.TeleportationOpponent,
                             M.sePiece=piece_,
                             M.sePosition=pos_ }
    return se_

seKingSidedCastling :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seKingSidedCastling = do
    _se <- (TPC.char '$')
    piece <- TP.option 'R' (TPC.char 'R')

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.KingSidedCastling,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_

seQueenSidedCastling :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seQueenSidedCastling = do
    _se <- (TPC.char '&')
    piece <- TP.option 'R' (TPC.char 'R')

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.QueenSidedCastling,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_

seSummoningOwn :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seSummoningOwn = do
    _se <- (TPC.char '<')
    piece <- TP.option ' ' TPC.upper

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.pieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.SummoningOwn,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_

seSummoningOpponent :: TP.Parsec String (C.LazyBox R.Rules) M.SideEffect
seSummoningOpponent = do
    _se <- (TPC.char '>')
    piece <- TP.option ' ' TPC.upper

    lb <- TP.getState
    let gameStatus_ = R.gameStatus $ C.unwrapLazyBox lb
    let piece_ = PP.opponentPieceByConsoleChar piece gameStatus_ -- $ GS.isLightOnMove gameStatus_
    let se_ = M.SideEffect { M.seType=M.SummoningOpponent,
                             M.sePiece=piece_,
                             M.sePosition=Pos.dummyPosition }
    return se_


-- parseMove :: String -> Either TP.ParseError M.Move
-- parseMove s = TP.parse move "fail" s

