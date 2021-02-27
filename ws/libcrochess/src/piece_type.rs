// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
// #[repr(i32)]
pub enum PieceType {
    DimStar = -15,

    DarkStarchild,
    DarkShaman,
    DarkSerpent,
    DarkCentaur,
    DarkWave,
    DarkUnicorn,
    DarkPyramid,
    DarkPegasus,
    DarkKing,
    DarkQueen,
    DarkRook,
    DarkBishop,
    DarkKnight,
    DarkPawn,

    None = 0,

    LightPawn,
    LightKnight,
    LightBishop,
    LightRook,
    LightQueen,
    LightKing,
    LightPegasus,
    LightPyramid,
    LightUnicorn,
    LightWave,
    LightCentaur,
    LightSerpent,
    LightShaman,
    LightStarchild,

    BrightStar,

    Monolith,
}

impl PieceType {

    pub fn from_symbol(c: char, is_light: bool) -> PieceType {
        return match c {
            ' ' => PieceType::None,

            'P' => if is_light { PieceType::LightPawn } else { PieceType::DarkPawn },
            'N' => if is_light { PieceType::LightKnight } else { PieceType::DarkKnight },
            'B' => if is_light { PieceType::LightBishop } else { PieceType::DarkBishop },
            'R' => if is_light { PieceType::LightRook } else { PieceType::DarkRook },
            'Q' => if is_light { PieceType::LightQueen } else { PieceType::DarkQueen },
            'K' => if is_light { PieceType::LightKing } else { PieceType::DarkKing },
            'G' => if is_light { PieceType::LightPegasus } else { PieceType::DarkPegasus },
            'A' => if is_light { PieceType::LightPyramid } else { PieceType::DarkPyramid },
            'U' => if is_light { PieceType::LightUnicorn } else { PieceType::DarkUnicorn },
            'W' => if is_light { PieceType::LightWave } else { PieceType::DarkWave },
            'C' => if is_light { PieceType::LightCentaur } else { PieceType::DarkCentaur },
            'S' => if is_light { PieceType::LightSerpent } else { PieceType::DarkSerpent },
            'H' => if is_light { PieceType::LightShaman } else { PieceType::DarkShaman },
            'I' => if is_light { PieceType::LightStarchild } else { PieceType::DarkStarchild },

            'T' => if is_light { PieceType::BrightStar } else { PieceType::DimStar },

            'M' => PieceType::Monolith,

            _ => PieceType::None,
        };
    }

    pub fn opposite(&self) -> PieceType {
        return match self {
            PieceType::DimStar => PieceType::BrightStar,

            PieceType::DarkStarchild => PieceType::LightStarchild,
            PieceType::DarkShaman => PieceType::LightShaman,
            PieceType::DarkSerpent => PieceType::LightSerpent,
            PieceType::DarkCentaur => PieceType::LightCentaur,
            PieceType::DarkWave => PieceType::LightWave,
            PieceType::DarkUnicorn => PieceType::LightUnicorn,
            PieceType::DarkPyramid => PieceType::LightPyramid,
            PieceType::DarkPegasus => PieceType::LightPegasus,
            PieceType::DarkKing => PieceType::LightKing,
            PieceType::DarkQueen => PieceType::LightQueen,
            PieceType::DarkRook => PieceType::LightRook,
            PieceType::DarkBishop => PieceType::LightBishop,
            PieceType::DarkKnight => PieceType::LightKnight,
            PieceType::DarkPawn => PieceType::LightPawn,

            PieceType::None => PieceType::None,

            PieceType::LightPawn => PieceType::DarkPawn,
            PieceType::LightKnight => PieceType::DarkKnight,
            PieceType::LightBishop => PieceType::DarkBishop,
            PieceType::LightRook => PieceType::DarkRook,
            PieceType::LightQueen => PieceType::DarkQueen,
            PieceType::LightKing => PieceType::DarkKing,
            PieceType::LightPegasus => PieceType::DarkPegasus,
            PieceType::LightPyramid => PieceType::DarkPyramid,
            PieceType::LightUnicorn => PieceType::DarkUnicorn,
            PieceType::LightWave => PieceType::DarkWave,
            PieceType::LightCentaur => PieceType::DarkCentaur,
            PieceType::LightSerpent => PieceType::DarkSerpent,
            PieceType::LightShaman => PieceType::DarkShaman,
            PieceType::LightStarchild => PieceType::DarkStarchild,

            PieceType::BrightStar => PieceType::DimStar,

            PieceType::Monolith => PieceType::Monolith,
        };
    }

    pub fn symbol(&self) -> char {
        return self.as_char().to_ascii_uppercase();
    }

    pub fn as_char(&self) -> char {
        return match self {
            PieceType::DimStar => 't',

            PieceType::DarkStarchild => 'i',
            PieceType::DarkShaman => 'h',
            PieceType::DarkSerpent => 's',
            PieceType::DarkCentaur => 'c',
            PieceType::DarkWave => 'w',
            PieceType::DarkUnicorn => 'u',
            PieceType::DarkPyramid => 'a',
            PieceType::DarkPegasus => 'g',
            PieceType::DarkKing => 'k',
            PieceType::DarkQueen => 'q',
            PieceType::DarkRook => 'r',
            PieceType::DarkBishop => 'b',
            PieceType::DarkKnight => 'n',
            PieceType::DarkPawn => 'p',

            PieceType::None => ' ',

            PieceType::LightPawn => 'P',
            PieceType::LightKnight => 'N',
            PieceType::LightBishop => 'B',
            PieceType::LightRook => 'R',
            PieceType::LightQueen => 'Q',
            PieceType::LightKing => 'K',
            PieceType::LightPegasus => 'G',
            PieceType::LightPyramid => 'A',
            PieceType::LightUnicorn => 'U',
            PieceType::LightWave => 'W',
            PieceType::LightCentaur => 'C',
            PieceType::LightSerpent => 'S',
            PieceType::LightShaman => 'H',
            PieceType::LightStarchild => 'I',

            PieceType::BrightStar => 'T',

            PieceType::Monolith => 'M',
        };
    }

    pub fn label(&self) ->&'static str {
        return match self {
            PieceType::DarkPawn | PieceType::LightPawn => "Pawn",
            PieceType::DarkKnight | PieceType::LightKnight => "Knight",
            PieceType::DarkBishop | PieceType::LightBishop => "Bishop",
            PieceType::DarkRook | PieceType::LightRook => "Rook",
            PieceType::DarkQueen | PieceType::LightQueen => "Queen",
            PieceType::DarkKing | PieceType::LightKing => "King",
            PieceType::DarkPegasus | PieceType::LightPegasus => "Pegasus",
            PieceType::DarkPyramid | PieceType::LightPyramid => "Pyramid",
            PieceType::DarkUnicorn | PieceType::LightUnicorn => "Unicorn",
            PieceType::DarkWave | PieceType::LightWave => "Wave",
            PieceType::DarkCentaur | PieceType::LightCentaur => "Centaur",
            PieceType::DarkSerpent | PieceType::LightSerpent => "Serpent",
            PieceType::DarkShaman | PieceType::LightShaman => "Shaman",
            PieceType::DarkStarchild | PieceType::LightStarchild => "Starchild",

            PieceType::DimStar | PieceType::BrightStar => "Star",

            PieceType::Monolith => "Monolith",

            PieceType::None  => "",
        };
    }

    pub fn is_dark(&self) -> bool {
        return match self {
            PieceType::DarkStarchild |
            PieceType::DarkShaman |
            PieceType::DarkSerpent |
            PieceType::DarkCentaur |
            PieceType::DarkWave |
            PieceType::DarkUnicorn |
            PieceType::DarkPyramid |
            PieceType::DarkPegasus |
            PieceType::DarkKing |
            PieceType::DarkQueen |
            PieceType::DarkRook |
            PieceType::DarkBishop |
            PieceType::DarkKnight |
            PieceType::DarkPawn => true,

            _ => false,
        };
    }

    pub fn is_light(&self) -> bool {
        return match self {
            PieceType::LightPawn |
            PieceType::LightKnight |
            PieceType::LightBishop |
            PieceType::LightRook |
            PieceType::LightQueen |
            PieceType::LightKing |
            PieceType::LightPegasus |
            PieceType::LightPyramid |
            PieceType::LightUnicorn |
            PieceType::LightWave |
            PieceType::LightCentaur |
            PieceType::LightSerpent |
            PieceType::LightShaman |
            PieceType::LightStarchild => true,

            _ => false,
        };
    }

    pub fn is_none(&self) -> bool {
        return self == &PieceType::None;
    }

}

impl fmt::Display for PieceType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", self.as_char());
    }
}
