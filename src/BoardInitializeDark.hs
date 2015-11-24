
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module BoardInitializeDark
where

import qualified Common as C
import qualified PieceType as PT
import qualified BoardType as BT
import qualified Position as Pos
import Common ( (<:>) )


initialDarkPawnsRowIndex :: BT.BoardType -> Int
initialDarkPawnsRowIndex bt = (BT.boardSize bt) - 2

initialDarkPawnsRank :: BT.BoardType -> [ PT.PieceType ]
initialDarkPawnsRank BT.None = []
initialDarkPawnsRank bt = [ PT.DarkPawn | _ <- [0 .. (BT.boardSize bt) - 1] ]

initialDarkFiguresRowIndex :: BT.BoardType -> Int
initialDarkFiguresRowIndex bt = (BT.boardSize bt) - 1

initialDarkFiguresRank :: BT.BoardType -> [ PT.PieceType ]
initialDarkFiguresRank BT.None = []
initialDarkFiguresRank bt = case bt of
    BT.None                    -> []

    BT.OddClassical            -> [ PT.DarkRook,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkRook ]

    BT.Classical               -> [ PT.DarkRook,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkRook ]

    BT.OddCroatianTies         -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.CroatianTies            -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.OddMayanAscendancy      -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.MayanAscendancy         -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.OddAgeOfAquarius        -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.AgeOfAquarius           -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.OddMirandasVeil         -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.MirandasVeil            -> [ PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook ]

    BT.OddNineteen             -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.Nineteen                -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.OddHemerasDawn          -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.HemerasDawn             -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.OddTamoanchanRevisited  -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.TamoanchanRevisited     -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.OddConquestOfTlalocan   -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkShaman,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkShaman,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.ConquestOfTlalocan      -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkShaman,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkShaman,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.OddDiscovery            -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkShaman,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkShaman,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.Discovery               -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkShaman,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkShaman,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.OddOne                  -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkShaman,
                                    PT.DarkStarchild,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkStarchild,
                                    PT.DarkShaman,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]

    BT.One                     -> [ PT.LightStar,
                                    PT.DarkRook,
                                    PT.DarkPegasus,
                                    PT.DarkPyramid,
                                    PT.DarkShaman,
                                    PT.DarkStarchild,
                                    PT.DarkUnicorn,
                                    PT.DarkWave,
                                    PT.DarkCentaur,
                                    PT.DarkSerpent,
                                    PT.DarkKnight,
                                    PT.DarkBishop,
                                    PT.DarkQueen,
                                    PT.DarkKing,
                                    PT.DarkBishop,
                                    PT.DarkKnight,
                                    PT.DarkSerpent,
                                    PT.DarkCentaur,
                                    PT.DarkWave,
                                    PT.DarkUnicorn,
                                    PT.DarkStarchild,
                                    PT.DarkShaman,
                                    PT.DarkPyramid,
                                    PT.DarkPegasus,
                                    PT.DarkRook,
                                    PT.DarkStar ]


darkKingInitialPosition :: BT.BoardType -> Maybe Pos.PiecePosition
darkKingInitialPosition bt = res
    where midx = C.firstIndex PT.DarkKing $ initialDarkFiguresRank bt
          r    = initialDarkFiguresRowIndex bt
          res  = case midx of
                 Nothing -> Nothing
                 Just idx -> Just (PT.DarkKing, (idx, r))

darkRookInitialPosition :: BT.BoardType -> Int -> Maybe Pos.PiecePosition
darkRookInitialPosition bt stepOver = res
    where midx = C.numberedIndex PT.DarkRook (initialDarkFiguresRank bt) stepOver
          r    = initialDarkFiguresRowIndex bt
          res  = case midx of
                 Nothing -> Nothing
                 Just idx -> Just (PT.DarkRook, (idx, r))

darkCastlingFiguresInitialPositions :: BT.BoardType -> [ Pos.PiecePosition ]
darkCastlingFiguresInitialPositions bt = mk <:> mr0 <:> mr1 <:> []
    where mk  = darkKingInitialPosition bt
          mr0 = darkRookInitialPosition bt 0
          mr1 = darkRookInitialPosition bt 1

