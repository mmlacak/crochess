
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module PieceType
where

import qualified Dict as D


type PieceTypeChar = Char


data PieceType  = DarkStarchild
                | DarkShaman
                | DarkSerpent
                | DarkCentaur
                | DarkStar
                | DarkWave
                | DarkUnicorn
                | DarkPyramid
                | DarkPegasus
                | DarkKing
                | DarkQueen
                | DarkRook
                | DarkKnight
                | DarkBishop
                | DarkPawn
                | None
                | Monolith
                | LightPawn
                | LightBishop
                | LightKnight
                | LightRook
                | LightQueen
                | LightKing
                | LightPegasus
                | LightPyramid
                | LightUnicorn
                | LightWave
                | LightStar
                | LightCentaur
                | LightSerpent
                | LightShaman
                | LightStarchild
    deriving (Show, Read, Eq, Bounded, Ord, Enum)


pieceSymbol :: PieceType -> PieceTypeChar
pieceSymbol pt = D.findByKey pt dict_
    where dict_ = [ ( None,             ' ' ),
                    ( Monolith,         'M' ),
                    ( DarkPawn,         'P' ),
                    ( LightPawn,        'P' ),
                    ( DarkBishop,       'B' ),
                    ( LightBishop,      'B' ),
                    ( DarkKnight,       'N' ),
                    ( LightKnight,      'N' ),
                    ( DarkRook,         'R' ),
                    ( LightRook,        'R' ),
                    ( DarkQueen,        'Q' ),
                    ( LightQueen,       'Q' ),
                    ( DarkKing,         'K' ),
                    ( LightKing,        'K' ),
                    ( DarkPegasus,      'G' ),
                    ( LightPegasus,     'G' ),
                    ( DarkPyramid,      'A' ),
                    ( LightPyramid,     'A' ),
                    ( DarkUnicorn,      'U' ),
                    ( LightUnicorn,     'U' ),
                    ( DarkWave,         'W' ),
                    ( LightWave,        'W' ),
                    ( DarkStar,         'T' ),
                    ( LightStar,        'T' ),
                    ( DarkCentaur,      'C' ),
                    ( LightCentaur,     'C' ),
                    ( DarkSerpent,      'S' ),
                    ( LightSerpent,     'S' ),
                    ( DarkShaman,       'H' ),
                    ( LightShaman,      'H' ),
                    ( DarkStarchild,    'I' ),
                    ( LightStarchild,   'I' )
                  ]

pieceName :: PieceType -> String
pieceName pt  = D.findByKey pt dict_
    where dict_ = [ ( None,             "" ),
                    ( Monolith,         "Monolith" ),
                    ( DarkPawn,         "Pawn" ),
                    ( LightPawn,        "Pawn" ),
                    ( DarkBishop,       "Bishop" ),
                    ( LightBishop,      "Bishop" ),
                    ( DarkKnight,       "Knight" ),
                    ( LightKnight,      "Knight" ),
                    ( DarkRook,         "Rook" ),
                    ( LightRook,        "Rook" ),
                    ( DarkQueen,        "Queen" ),
                    ( LightQueen,       "Queen" ),
                    ( DarkKing,         "King" ),
                    ( LightKing,        "King" ),
                    ( DarkPegasus,      "Pegasus" ),
                    ( LightPegasus,     "Pegasus" ),
                    ( DarkPyramid,      "Pyramid" ),
                    ( LightPyramid,     "Pyramid" ),
                    ( DarkUnicorn,      "Unicorn" ),
                    ( LightUnicorn,     "Unicorn" ),
                    ( DarkWave,         "Wave" ),
                    ( LightWave,        "Wave" ),
                    ( DarkStar,         "Star" ),
                    ( LightStar,        "Star" ),
                    ( DarkCentaur,      "Centaur" ),
                    ( LightCentaur,     "Centaur" ),
                    ( DarkSerpent,      "Serpent" ),
                    ( LightSerpent,     "Serpent" ),
                    ( DarkShaman,       "Shaman" ),
                    ( LightShaman,      "Shaman" ),
                    ( DarkStarchild,    "Starchild" ),
                    ( LightStarchild,   "Starchild" )
                  ]

oppositeColorPiece :: PieceType -> PieceType
oppositeColorPiece pt = D.findByKey pt dict_
    where dict_ = [ ( None,             None ),
                    ( Monolith,         Monolith ),
                    ( DarkPawn,         LightPawn ),
                    ( LightPawn,        DarkPawn ),
                    ( DarkBishop,       LightBishop ),
                    ( LightBishop,      DarkBishop ),
                    ( DarkKnight,       LightKnight ),
                    ( LightKnight,      DarkKnight ),
                    ( DarkRook,         LightRook ),
                    ( LightRook,        DarkRook ),
                    ( DarkQueen,        LightQueen ),
                    ( LightQueen,       DarkQueen ),
                    ( DarkKing,         LightKing ),
                    ( LightKing,        DarkKing ),
                    ( DarkPegasus,      LightPegasus ),
                    ( LightPegasus,     DarkPegasus ),
                    ( DarkPyramid,      LightPyramid ),
                    ( LightPyramid,     DarkPyramid ),
                    ( DarkUnicorn,      LightUnicorn ),
                    ( LightUnicorn,     DarkUnicorn ),
                    ( DarkWave,         LightWave ),
                    ( LightWave,        DarkWave ),
                    ( DarkStar,         LightStar ),
                    ( LightStar,        DarkStar ),
                    ( DarkCentaur,      LightCentaur ),
                    ( LightCentaur,     DarkCentaur ),
                    ( DarkSerpent,      LightSerpent ),
                    ( LightSerpent,     DarkSerpent ),
                    ( DarkShaman,       LightShaman ),
                    ( LightShaman,      DarkShaman ),
                    ( DarkStarchild,    LightStarchild ),
                    ( LightStarchild,   DarkStarchild )
                  ]

isPieceLight :: PieceType -> Bool
isPieceLight pt = pt `elem` [ LightPawn,
                              LightBishop,
                              LightKnight,
                              LightRook,
                              LightQueen,
                              LightKing,
                              LightPegasus,
                              LightPyramid,
                              LightUnicorn,
                              LightWave,
                              LightStar,
                              LightCentaur,
                              LightSerpent,
                              LightShaman,
                              LightStarchild ]

isPieceDark :: PieceType -> Bool
isPieceDark pt = pt `elem` [ DarkStarchild,
                             DarkShaman,
                             DarkSerpent,
                             DarkCentaur,
                             DarkStar,
                             DarkWave,
                             DarkUnicorn,
                             DarkPyramid,
                             DarkPegasus,
                             DarkKing,
                             DarkQueen,
                             DarkRook,
                             DarkKnight,
                             DarkBishop,
                             DarkPawn ]

isPieceColored :: PieceType -> Bool
isPieceColored pt = (isPieceLight pt) || (isPieceDark pt)

isPieceNone :: PieceType -> Bool
isPieceNone pt = pt == None

isPieceMonolith :: PieceType -> Bool
isPieceMonolith pt = pt == Monolith

isPieceTheSameKind :: PieceType -> PieceType -> Bool
isPieceTheSameKind pt1 pt2 = (pt1 == pt2) || (pt1 == oppositeColorPiece pt2)

isPieceTheSameColor :: PieceType -> PieceType -> Bool
isPieceTheSameColor pt1 pt2 | isPieceLight pt1      = isPieceLight pt2
                            | isPieceDark pt1       = isPieceDark pt2
                            | isPieceMonolith pt1   = isPieceMonolith pt2
                            | isPieceNone pt1       = isPieceNone pt2
                            | otherwise             = False

pieceFullName :: PieceType -> String
pieceFullName pt | isPieceLight pt = "Light " ++ pieceName pt
                 | isPieceDark pt  = "Dark " ++ pieceName pt
                 | otherwise       = pieceName pt

isPieceSymbol :: PieceTypeChar -> Bool
isPieceSymbol c = c `elem` "MPBKRQKGAUWTCSHI"

