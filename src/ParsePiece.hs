
-- Copyright (c) 2014, 2015, 2019 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module ParsePiece
where

import qualified Data.Char as DC

import qualified Dict as D
import qualified GameStatus as GS
import qualified PieceType as PT


consolePieceSymbol :: PT.PieceType -> PT.PieceTypeChar
consolePieceSymbol pt = D.findByKey pt dict_
    where dict_ = [ ( PT.None,              ' ' ),
                    ( PT.Monolith,          'M' ),
                    ( PT.DarkPawn,          'p' ),
                    ( PT.LightPawn,         'P' ),
                    ( PT.DarkBishop,        'b' ),
                    ( PT.LightBishop,       'B' ),
                    ( PT.DarkKnight,        'n' ),
                    ( PT.LightKnight,       'N' ),
                    ( PT.DarkRook,          'r' ),
                    ( PT.LightRook,         'R' ),
                    ( PT.DarkQueen,         'q' ),
                    ( PT.LightQueen,        'Q' ),
                    ( PT.DarkKing,          'k' ),
                    ( PT.LightKing,         'K' ),
                    ( PT.DarkPegasus,       'g' ),
                    ( PT.LightPegasus,      'G' ),
                    ( PT.DarkPyramid,       'a' ),
                    ( PT.LightPyramid,      'A' ),
                    ( PT.DarkUnicorn,       'u' ),
                    ( PT.LightUnicorn,      'U' ),
                    ( PT.DarkWave,          'w' ),
                    ( PT.LightWave,         'W' ),
                    ( PT.DarkStar,          't' ),
                    ( PT.LightStar,         'T' ),
                    ( PT.DarkCentaur,       'c' ),
                    ( PT.LightCentaur,      'C' ),
                    ( PT.DarkSerpent,       's' ),
                    ( PT.LightSerpent,      'S' ),
                    ( PT.DarkShaman,        'h' ),
                    ( PT.LightShaman,       'H' ),
                    ( PT.DarkStarchild,     'i' ),
                    ( PT.LightStarchild,    'I' )
                  ]

isConsolePieceSymbol :: Char -> Bool
isConsolePieceSymbol c = c `elem` "MPBKRQKGAUWTCSHIpbkrqkgauwtcshi"

pieceByConsoleSymbol :: Char -> PT.PieceType
pieceByConsoleSymbol sym | D.isKeyInDict sym dict_ = D.findByKey sym dict_
                         | otherwise               = PT.None
    where dict_ = [ ( ' ', PT.None ),
                    ( 'M', PT.Monolith ),
                    ( 'p', PT.DarkPawn ),
                    ( 'P', PT.LightPawn ),
                    ( 'b', PT.DarkBishop ),
                    ( 'B', PT.LightBishop ),
                    ( 'n', PT.DarkKnight ),
                    ( 'N', PT.LightKnight ),
                    ( 'r', PT.DarkRook ),
                    ( 'R', PT.LightRook ),
                    ( 'q', PT.DarkQueen ),
                    ( 'Q', PT.LightQueen ),
                    ( 'k', PT.DarkKing ),
                    ( 'K', PT.LightKing ),
                    ( 'g', PT.DarkPegasus ),
                    ( 'G', PT.LightPegasus ),
                    ( 'a', PT.DarkPyramid ),
                    ( 'A', PT.LightPyramid ),
                    ( 'u', PT.DarkUnicorn ),
                    ( 'U', PT.LightUnicorn ),
                    ( 'w', PT.DarkWave ),
                    ( 'W', PT.LightWave ),
                    ( 't', PT.DarkStar ),
                    ( 'T', PT.LightStar ),
                    ( 'c', PT.DarkCentaur ),
                    ( 'C', PT.LightCentaur ),
                    ( 's', PT.DarkSerpent ),
                    ( 'S', PT.LightSerpent ),
                    ( 'h', PT.DarkShaman ),
                    ( 'H', PT.LightShaman ),
                    ( 'i', PT.DarkStarchild ),
                    ( 'I', PT.LightStarchild )
                  ]

-- consolePieceChar :: PT.PieceType -> PT.PieceTypeChar
-- consolePieceChar pt = D.findByKey pt dict_
    -- where dict_ = [ ( PT.None,              ' ' ),
                    -- ( PT.Monolith,          'M' ),
                    -- ( PT.DarkPawn,          'P' ),
                    -- ( PT.LightPawn,         'P' ),
                    -- ( PT.DarkBishop,        'B' ),
                    -- ( PT.LightBishop,       'B' ),
                    -- ( PT.DarkKnight,        'N' ),
                    -- ( PT.LightKnight,       'N' ),
                    -- ( PT.DarkRook,          'R' ),
                    -- ( PT.LightRook,         'R' ),
                    -- ( PT.DarkQueen,         'Q' ),
                    -- ( PT.LightQueen,        'Q' ),
                    -- ( PT.DarkKing,          'K' ),
                    -- ( PT.LightKing,         'K' ),
                    -- ( PT.DarkPegasus,       'G' ),
                    -- ( PT.LightPegasus,      'G' ),
                    -- ( PT.DarkPyramid,       'A' ),
                    -- ( PT.LightPyramid,      'A' ),
                    -- ( PT.DarkUnicorn,       'U' ),
                    -- ( PT.LightUnicorn,      'U' ),
                    -- ( PT.DarkWave,          'W' ),
                    -- ( PT.LightWave,         'W' ),
                    -- ( PT.DarkStar,          'T' ),
                    -- ( PT.LightStar,         'T' ),
                    -- ( PT.DarkCentaur,       'C' ),
                    -- ( PT.LightCentaur,      'C' ),
                    -- ( PT.DarkSerpent,       'S' ),
                    -- ( PT.LightSerpent,      'S' ),
                    -- ( PT.DarkShaman,        'H' ),
                    -- ( PT.LightShaman,       'H' ),
                    -- ( PT.DarkStarchild,     'I' ),
                    -- ( PT.LightStarchild,    'I' )
                  -- ]

consolePieceChar :: PT.PieceType -> PT.PieceTypeChar
consolePieceChar pt = DC.toUpper $ consolePieceSymbol pt

isConsolePieceChar :: Char -> Bool
isConsolePieceChar c = c `elem` "MPBKRQKGAUWTCSHI"

consolePieceByChar :: Char -> PT.PieceTypeChar
consolePieceByChar c = if isConsolePieceChar c then c else 'P'

pieceByConsoleChar :: Char -> GS.GameStatus -> PT.PieceType
pieceByConsoleChar c gameStatus | D.isKeyInDict c dict_ = D.findByKey c dict_
                                | otherwise             = PT.None
    where light_ = [ ( ' ', PT.None ),
                     ( 'M', PT.Monolith ),
                     ( 'P', PT.LightPawn ),
                     ( 'B', PT.LightBishop ),
                     ( 'N', PT.LightKnight ),
                     ( 'R', PT.LightRook ),
                     ( 'Q', PT.LightQueen ),
                     ( 'K', PT.LightKing ),
                     ( 'G', PT.LightPegasus ),
                     ( 'A', PT.LightPyramid ),
                     ( 'U', PT.LightUnicorn ),
                     ( 'W', PT.LightWave ),
                     ( 'T', PT.LightStar ),
                     ( 'C', PT.LightCentaur ),
                     ( 'S', PT.LightSerpent ),
                     ( 'H', PT.LightShaman ),
                     ( 'I', PT.LightStarchild )
                   ]
          dark_ =  [ ( ' ', PT.None ),
                     ( 'M', PT.Monolith ),
                     ( 'P', PT.DarkPawn ),
                     ( 'B', PT.DarkBishop ),
                     ( 'N', PT.DarkKnight ),
                     ( 'R', PT.DarkRook ),
                     ( 'Q', PT.DarkQueen ),
                     ( 'K', PT.DarkKing ),
                     ( 'G', PT.DarkPegasus ),
                     ( 'A', PT.DarkPyramid ),
                     ( 'U', PT.DarkUnicorn ),
                     ( 'W', PT.DarkWave ),
                     ( 'T', PT.DarkStar ),
                     ( 'C', PT.DarkCentaur ),
                     ( 'S', PT.DarkSerpent ),
                     ( 'H', PT.DarkShaman ),
                     ( 'I', PT.DarkStarchild )
                   ]
          dict_ = case gameStatus of GS.LightOnMove -> light_
                                     GS.DarkOnMove  -> dark_
                                     _              -> []

opponentPieceByConsoleChar :: Char -> GS.GameStatus -> PT.PieceType
opponentPieceByConsoleChar c gs = pieceByConsoleChar c $ GS.reverseOnMove gs


