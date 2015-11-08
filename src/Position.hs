
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- See accompanying LICENSE.txt for details.

module Position
where

import qualified PieceType as PT


type Index = Int
type ColumnIndex = Index
type RowIndex = Index
type Position = (ColumnIndex, RowIndex)
type Positions = [ Position ]

dummyIndex :: Index
dummyIndex = -42

dummyColumnIndex :: ColumnIndex
dummyColumnIndex = -42

dummyRowIndex :: RowIndex
dummyRowIndex = -42

dummyPosition :: (ColumnIndex, RowIndex)
dummyPosition = (dummyColumnIndex, dummyRowIndex)


type PieceColumnIndex = (PT.PieceType, ColumnIndex)
type PieceRowIndex = (PT.PieceType, RowIndex)
type PiecePosition = (PT.PieceType, Position)
type PieceStartEndPosition = (PT.PieceType, Position, Position)

dummyPieceColumnIndex :: PieceColumnIndex
dummyPieceColumnIndex = (PT.None, dummyColumnIndex)

dummyPieceRowIndex :: PieceRowIndex
dummyPieceRowIndex = (PT.None, dummyRowIndex)

dummyPiecePosition :: PiecePosition
dummyPiecePosition = (PT.None, dummyPosition)

dummyPieceStartEndPosition :: PieceStartEndPosition
dummyPieceStartEndPosition = (PT.None, dummyPosition, dummyPosition)


type PiecePositions = [ PiecePosition ]
type SetPiecesList = PiecePositions

type ClearField = Position
type ClearFieldsList = [ ClearField ]

convertToClearField :: PiecePosition -> ClearField
convertToClearField (pt, pos) = pos

convertToPiecePosition :: ClearField -> PiecePosition
convertToPiecePosition cf = (PT.None, cf)

convertToClearFieldsList :: PiecePositions -> ClearFieldsList
convertToClearFieldsList pposs = map convertToClearField pposs

convertToPiecePositions :: ClearFieldsList -> PiecePositions
convertToPiecePositions cfl = map convertToPiecePosition cfl

