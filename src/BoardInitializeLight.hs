
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module BoardInitializeLight
where

import qualified Common as C
import qualified PieceType as PT
import qualified BoardType as BT
import qualified Position as Pos
import Common ( (<:>) )


initialLightPawnsRowIndex :: BT.BoardType -> Int
initialLightPawnsRowIndex _ = 1

initialLightPawnsRank :: BT.BoardType -> [ PT.PieceType ]
initialLightPawnsRank BT.None = []
initialLightPawnsRank bt = [ PT.LightPawn | _ <- [0 .. (BT.boardSize bt) - 1] ]

initialLightFiguresRowIndex :: BT.BoardType -> Int
initialLightFiguresRowIndex _ = 0

initialLightFiguresRank :: BT.BoardType -> [ PT.PieceType ]
initialLightFiguresRank BT.None = []
initialLightFiguresRank bt = case bt of
    BT.None                    -> []

    BT.OddClassical            -> [ PT.LightRook,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightRook ]

    BT.Classical               -> [ PT.LightRook,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightRook ]

    BT.OddCroatianTies         -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.CroatianTies            -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.OddMayanAscendancy      -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.MayanAscendancy         -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.OddAgeOfAquarius        -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.AgeOfAquarius           -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.OddMirandasVeil         -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.MirandasVeil            -> [ PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook ]

    BT.OddNineteen             -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.Nineteen                -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.OddHemerasDawn          -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.HemerasDawn             -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.OddTamoanchanRevisited  -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.TamoanchanRevisited     -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.OddConquestOfTlalocan   -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightShaman,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightShaman,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.ConquestOfTlalocan      -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightShaman,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightShaman,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.OddDiscovery            -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightShaman,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightShaman,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.Discovery               -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightShaman,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightShaman,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.OddOne                  -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightShaman,
                                    PT.LightStarchild,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightStarchild,
                                    PT.LightShaman,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]

    BT.One                     -> [ PT.DarkStar,
                                    PT.LightRook,
                                    PT.LightPegasus,
                                    PT.LightPyramid,
                                    PT.LightShaman,
                                    PT.LightStarchild,
                                    PT.LightUnicorn,
                                    PT.LightWave,
                                    PT.LightCentaur,
                                    PT.LightSerpent,
                                    PT.LightKnight,
                                    PT.LightBishop,
                                    PT.LightQueen,
                                    PT.LightKing,
                                    PT.LightBishop,
                                    PT.LightKnight,
                                    PT.LightSerpent,
                                    PT.LightCentaur,
                                    PT.LightWave,
                                    PT.LightUnicorn,
                                    PT.LightStarchild,
                                    PT.LightShaman,
                                    PT.LightPyramid,
                                    PT.LightPegasus,
                                    PT.LightRook,
                                    PT.LightStar ]


lightKingInitialPosition :: BT.BoardType -> Maybe Pos.PiecePosition
lightKingInitialPosition bt = res
    where midx = C.firstIndex PT.LightKing $ initialLightFiguresRank bt
          r    = initialLightFiguresRowIndex bt
          res  = case midx of
                 Nothing -> Nothing
                 Just idx -> Just (PT.LightKing, (idx, r))

lightRookInitialPosition :: BT.BoardType -> Int -> Maybe Pos.PiecePosition
lightRookInitialPosition bt stepOver = res
    where midx = C.numberedIndex PT.LightRook (initialLightFiguresRank bt) stepOver
          r    = initialLightFiguresRowIndex bt
          res  = case midx of
                 Nothing -> Nothing
                 Just idx -> Just (PT.LightRook, (idx, r))

lightCastlingFiguresInitialPositions :: BT.BoardType -> [ Pos.PiecePosition ]
lightCastlingFiguresInitialPositions bt = mk <:> mr0 <:> mr1 <:> []
    where mk  = lightKingInitialPosition bt
          mr0 = lightRookInitialPosition bt 0
          mr1 = lightRookInitialPosition bt 1

