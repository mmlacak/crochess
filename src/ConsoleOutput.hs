
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module ConsoleOutput
where

import qualified Data.Char as DC
import qualified Data.List as DL

import qualified PieceType as PT
import qualified BoardType as BT
import qualified GameStatus as GS
import qualified Board as B
import qualified Position as Pos
import qualified Rules as R
import qualified Game as G


type FieldChar = Char
type FieldStr = String


fieldChar :: PT.PieceType -> FieldChar
fieldChar pt = if PT.isPieceNone pt then lightField
               else if PT.isPieceDark pt then DC.toLower ps_
               else ps_
    where ps_ = PT.pieceSymbol pt

fieldCharWithShade :: BT.BoardType -> PT.PieceType -> Pos.ColumnIndex -> Pos.RowIndex -> FieldChar
fieldCharWithShade bt pt fi ri =
        if PT.isPieceNone pt then fieldShadeChar bt fi ri
        else if PT.isPieceDark pt then DC.toLower ps_
        else ps_
    where ps_ = PT.pieceSymbol pt

fieldShadeChar :: BT.BoardType -> Pos.ColumnIndex -> Pos.RowIndex -> FieldChar
fieldShadeChar bt fi ri = if B.isFieldLight bt fi ri then lightField
                                                     else darkField

fieldSeparator :: FieldChar
fieldSeparator = ' '

rankSeparator :: FieldStr
rankSeparator = " | "

fileSeparator :: FieldChar
fileSeparator = '-'

fileMarkIntro1 :: FieldStr
fileMarkIntro1 = "    "

fileMarkIntro2 :: FieldStr
fileMarkIntro2 = "     "

lightField :: FieldChar
lightField = '.'

darkField :: FieldChar
darkField = ','

playGroundString :: B.PlayGround -> FieldStr
playGroundString pg = "\n" ++ (unlines d_) ++ e_ ++ "\n"
    where s_ = [ [ fieldChar piece_ | piece_ <- rank_ ] | rank_ <- (DL.transpose pg) ]
          d_ = [ (if ri_ < 10 then " " else "")
                 ++ (show ri_)
                 ++ rankSeparator
                 ++ (DL.intersperse fieldSeparator rank_)
                 | (rank_, ri_) <- zip (DL.reverse s_) [length s_, length s_ - 1 ..] ]
          e_ = fileMarkIntro1
               ++ replicate (2 * length s_) fileSeparator
               ++ "\n"
               ++ fileMarkIntro2
               ++ take (2 * length s_) (DL.intersperse fieldSeparator ['a' .. 'z'])

boardString :: B.Board -> FieldStr
boardString b = "\n" ++ (unlines d_) ++ e_ ++ "\n"
    where pg_ = B.playGround b
          bt_ = B.boardType b
          s_ = [ [ fieldCharWithShade bt_ piece_ fi_ ri_
                   | (piece_, ri_) <- zip rank_ [length pg_, length pg_ - 1 ..] ]
                 | (rank_, fi_) <- zip (DL.transpose pg_) [0 ..] ]
          d_ = [ (if ri_ < 10 then " " else "")
                 ++ (show ri_)
                 ++ rankSeparator
                 ++ (DL.intersperse fieldSeparator rank_)
                 | (rank_, ri_) <- zip (DL.reverse s_) [length s_, length s_ - 1 ..] ]
          e_ = fileMarkIntro1
               ++ replicate (2 * length s_) fileSeparator
               ++ "\n"
               ++ fileMarkIntro2
               ++ take (2 * length s_) (DL.intersperse fieldSeparator ['a' .. 'z'])

gameString :: G.Game -> String
gameString g@(G.Game r) = b_ ++ "\n" ++ gs_ ++ "\n"
    where b_ = boardString $ R.board r
          gs_ = GS.gameStatusString $ R.gameStatus r
-- TODO :: add last move! use short alg output

-- TODO :: add move short alg output (and fully-qualified long)!

