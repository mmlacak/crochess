
pub mod piece_type {

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

    pub fn valid(e: PieceType) -> bool {
        return PieceType::DarkStarchild <= e && e >= PieceType::LightStarchild;
    }

    pub fn opposite(e: PieceType) -> PieceType {
        return match e {
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

    pub fn symbol(e: PieceType) -> char {
        return match e {
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

    pub fn label(e: PieceType) ->&'static str {
        return match e {
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

    pub fn is_dark(e: PieceType) -> bool {
        return e < PieceType::None;
    }

    pub fn is_light(e: PieceType) -> bool {
        return e > PieceType::None;
    }

    pub fn is_none(e: PieceType) -> bool {
        return e == PieceType::None;
    }

}
