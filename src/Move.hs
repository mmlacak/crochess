
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module Move
where

import qualified PieceType as PT
import qualified Position as Pos


data SideEffectType = NoSideEffect
                    | Promotion
                    | Conversion
                    | Capture
                    | EnPassant
                    | TeleportationOwn
                    | TeleportationOpponent
                    | KingSidedCastling
                    | QueenSidedCastling
                    | SummoningOwn
                    | SummoningOpponent
    deriving (Show, Read, Eq, Bounded, Ord, Enum)

data ConditionType  = NoCondition
                    | Check
                    | DoubleCheck
                    | CheckMate
    deriving (Show, Read, Eq, Bounded, Ord, Enum)


data SideEffect = SideEffect { seType :: SideEffectType,
                               sePiece :: PT.PieceType,
                               sePosition :: Pos.Position }
    deriving (Show, Read, Eq)

dummySideEffect :: SideEffect
dummySideEffect = SideEffect { seType=NoSideEffect,
                               sePiece=PT.None,
                               sePosition=Pos.dummyPosition }


data Ply = Ply { piece :: PT.PieceType,
                 start :: Pos.Position,
                 end :: Pos.Position,
                 sideEffect :: SideEffect }
    deriving (Show, Read, Eq)

dummyPly :: Ply
dummyPly = Ply { piece=PT.None,
                 start=Pos.dummyPosition,
                 end=Pos.dummyPosition,
                 sideEffect=dummySideEffect }


data Move = Move { plies :: [ Ply ],
                   condition :: ConditionType }
    deriving (Show, Read, Eq)

dummyMove :: Move
dummyMove = Move { plies=[],
                   condition=NoCondition }


isPlyCascading :: Ply -> Ply -> Bool
isPlyCascading ply1@(Ply p1 s1 e1 se1) ply2@(Ply p2 s2 e2 se2) = e1 == s2

fetchClearField :: Ply -> Pos.ClearField
fetchClearField (Ply p s e se) = s

fetchPiecePosition :: Ply -> Pos.PiecePosition
fetchPiecePosition (Ply p s e se) = (p, e)

fetchAllPiecePositions :: Move -> Pos.PiecePositions
fetchAllPiecePositions m@(Move ps cond) = reverse $ fetchReversedPiecePositions $ reverse ps

fetchReversedPiecePositions :: [ Ply ] -> Pos.PiecePositions
fetchReversedPiecePositions [] = []
fetchReversedPiecePositions [p] = [r_, pp_]
    where pp_ = Pos.convertToPiecePosition $ fetchClearField p
          r_ = fetchPiecePosition p
fetchReversedPiecePositions (p1:p2:ps) = if c_ then r_ : fetchReversedPiecePositions (p2:ps)
                                               else r_ : pp_ : fetchReversedPiecePositions (p2:ps)
    where c_ = isPlyCascading p2 p1
          pp_ = Pos.convertToPiecePosition $ fetchClearField p1
          r_ = fetchPiecePosition p1


type MoveList = [ Move ]

appendMove :: MoveList -> Move -> MoveList
appendMove ml m = ml ++ [ m ]

