// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
// #[repr(i32)]
pub enum PieceType {
    DarkStarchild = -16,
    DarkMonolith,
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
    LightStar,
    LightCentaur,
    LightSerpent,
    LightShaman,
    LightMonolith,
    LightStarchild,
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
            'T' => if is_light { PieceType::LightStar } else { PieceType::DarkStar },
            'C' => if is_light { PieceType::LightCentaur } else { PieceType::DarkCentaur },
            'S' => if is_light { PieceType::LightSerpent } else { PieceType::DarkSerpent },
            'H' => if is_light { PieceType::LightShaman } else { PieceType::DarkShaman },
            'M' => if is_light { PieceType::LightMonolith } else { PieceType::DarkMonolith },
            'I' => if is_light { PieceType::LightStarchild } else { PieceType::DarkStarchild },

            _ => PieceType::None,
        };
    }

    pub fn opposite(&self) -> PieceType {
        return match self {
            PieceType::DarkStarchild => PieceType::LightStarchild,
            PieceType::DarkMonolith => PieceType::LightMonolith,
            PieceType::DarkShaman => PieceType::LightShaman,
            PieceType::DarkSerpent => PieceType::LightSerpent,
            PieceType::DarkCentaur => PieceType::LightCentaur,
            PieceType::DarkStar => PieceType::LightStar,
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
            PieceType::LightStar => PieceType::DarkStar,
            PieceType::LightCentaur => PieceType::DarkCentaur,
            PieceType::LightSerpent => PieceType::DarkSerpent,
            PieceType::LightShaman => PieceType::DarkShaman,
            PieceType::LightMonolith => PieceType::DarkMonolith,
            PieceType::LightStarchild => PieceType::DarkStarchild,
        };
    }

    pub fn symbol(&self) -> char {
        return self.as_char().to_ascii_uppercase();
    }

    pub fn as_char(&self) -> char {
        return match self {
            PieceType::DarkStarchild => 'i',
            PieceType::DarkMonolith => 'm',
            PieceType::DarkShaman => 'h',
            PieceType::DarkSerpent => 's',
            PieceType::DarkCentaur => 'c',
            PieceType::DarkStar => 't',
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
            PieceType::LightStar => 'T',
            PieceType::LightCentaur => 'C',
            PieceType::LightSerpent => 'S',
            PieceType::LightShaman => 'H',
            PieceType::LightMonolith => 'M',
            PieceType::LightStarchild => 'I',
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
            PieceType::DarkStar | PieceType::LightStar => "Star",
            PieceType::DarkCentaur | PieceType::LightCentaur => "Centaur",
            PieceType::DarkSerpent | PieceType::LightSerpent => "Serpent",
            PieceType::DarkShaman | PieceType::LightShaman => "Shaman",
            PieceType::DarkMonolith | PieceType::LightMonolith => "Monolith",
            PieceType::DarkStarchild | PieceType::LightStarchild => "Starchild",

            PieceType::None  => "",
        };
    }

    pub fn is_dark(&self) -> bool {
        return self < &PieceType::None;
    }

    pub fn is_light(&self) -> bool {
        return self > &PieceType::None;
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
