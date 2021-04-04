// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

import ascii = std.ascii;


export enum PieceType {
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


export PieceType from_symbol( char c, bool is_light ) {
    switch ( c ) {
        case ' ': return PieceType.None;

        case 'P': return ( is_light ) ? PieceType.LightPawn : PieceType.DarkPawn;
        case 'N': return ( is_light ) ? PieceType.LightKnight : PieceType.DarkKnight;
        case 'B': return ( is_light ) ? PieceType.LightBishop : PieceType.DarkBishop;
        case 'R': return ( is_light ) ? PieceType.LightRook : PieceType.DarkRook;
        case 'Q': return ( is_light ) ? PieceType.LightQueen : PieceType.DarkQueen;
        case 'K': return ( is_light ) ? PieceType.LightKing : PieceType.DarkKing;
        case 'G': return ( is_light ) ? PieceType.LightPegasus : PieceType.DarkPegasus;
        case 'A': return ( is_light ) ? PieceType.LightPyramid : PieceType.DarkPyramid;
        case 'U': return ( is_light ) ? PieceType.LightUnicorn : PieceType.DarkUnicorn;
        case 'W': return ( is_light ) ? PieceType.LightWave : PieceType.DarkWave;
        case 'C': return ( is_light ) ? PieceType.LightCentaur : PieceType.DarkCentaur;
        case 'S': return ( is_light ) ? PieceType.LightSerpent : PieceType.DarkSerpent;
        case 'H': return ( is_light ) ? PieceType.LightShaman : PieceType.DarkShaman;
        case 'I': return ( is_light ) ? PieceType.LightStarchild : PieceType.DarkStarchild;

        case 'T': return ( is_light ) ? PieceType.BrightStar : PieceType.DimStar;

        case 'M': return PieceType.Monolith;

        default: return PieceType.None;
    }
}

export PieceType opposite( PieceType pt ) {
    final switch ( pt ) {
        case PieceType.DimStar: return PieceType.BrightStar;

        case PieceType.DarkStarchild: return PieceType.LightStarchild;
        case PieceType.DarkShaman: return PieceType.LightShaman;
        case PieceType.DarkSerpent: return PieceType.LightSerpent;
        case PieceType.DarkCentaur: return PieceType.LightCentaur;
        case PieceType.DarkWave: return PieceType.LightWave;
        case PieceType.DarkUnicorn: return PieceType.LightUnicorn;
        case PieceType.DarkPyramid: return PieceType.LightPyramid;
        case PieceType.DarkPegasus: return PieceType.LightPegasus;
        case PieceType.DarkKing: return PieceType.LightKing;
        case PieceType.DarkQueen: return PieceType.LightQueen;
        case PieceType.DarkRook: return PieceType.LightRook;
        case PieceType.DarkBishop: return PieceType.LightBishop;
        case PieceType.DarkKnight: return PieceType.LightKnight;
        case PieceType.DarkPawn: return PieceType.LightPawn;

        case PieceType.None: return PieceType.None;

        case PieceType.LightPawn: return PieceType.DarkPawn;
        case PieceType.LightKnight: return PieceType.DarkKnight;
        case PieceType.LightBishop: return PieceType.DarkBishop;
        case PieceType.LightRook: return PieceType.DarkRook;
        case PieceType.LightQueen: return PieceType.DarkQueen;
        case PieceType.LightKing: return PieceType.DarkKing;
        case PieceType.LightPegasus: return PieceType.DarkPegasus;
        case PieceType.LightPyramid: return PieceType.DarkPyramid;
        case PieceType.LightUnicorn: return PieceType.DarkUnicorn;
        case PieceType.LightWave: return PieceType.DarkWave;
        case PieceType.LightCentaur: return PieceType.DarkCentaur;
        case PieceType.LightSerpent: return PieceType.DarkSerpent;
        case PieceType.LightShaman: return PieceType.DarkShaman;
        case PieceType.LightStarchild: return PieceType.DarkStarchild;

        case PieceType.BrightStar: return PieceType.DimStar;

        case PieceType.Monolith: return PieceType.Monolith;
    }
}

export char as_char( PieceType pt ) {
    final switch ( pt ) {
        case PieceType.DimStar: return 't';

        case PieceType.DarkStarchild: return 'i';
        case PieceType.DarkShaman: return 'h';
        case PieceType.DarkSerpent: return 's';
        case PieceType.DarkCentaur: return 'c';
        case PieceType.DarkWave: return 'w';
        case PieceType.DarkUnicorn: return 'u';
        case PieceType.DarkPyramid: return 'a';
        case PieceType.DarkPegasus: return 'g';
        case PieceType.DarkKing: return 'k';
        case PieceType.DarkQueen: return 'q';
        case PieceType.DarkRook: return 'r';
        case PieceType.DarkBishop: return 'b';
        case PieceType.DarkKnight: return 'n';
        case PieceType.DarkPawn: return 'p';

        case PieceType.None: return ' ';

        case PieceType.LightPawn: return 'P';
        case PieceType.LightKnight: return 'N';
        case PieceType.LightBishop: return 'B';
        case PieceType.LightRook: return 'R';
        case PieceType.LightQueen: return 'Q';
        case PieceType.LightKing: return 'K';
        case PieceType.LightPegasus: return 'G';
        case PieceType.LightPyramid: return 'A';
        case PieceType.LightUnicorn: return 'U';
        case PieceType.LightWave: return 'W';
        case PieceType.LightCentaur: return 'C';
        case PieceType.LightSerpent: return 'S';
        case PieceType.LightShaman: return 'H';
        case PieceType.LightStarchild: return 'I';

        case PieceType.BrightStar: return 'T';

        case PieceType.Monolith: return 'M';
    }
}

export char as_symbol( PieceType pt ) {
    return ascii.toUpper( as_char( pt ) );
}

export string label( PieceType pt ) {
    final switch ( pt ) {
        case PieceType.DarkPawn:
        case PieceType.LightPawn: return "Pawn";

        case PieceType.DarkKnight:
        case PieceType.LightKnight: return "Knight";

        case PieceType.DarkBishop:
        case PieceType.LightBishop: return "Bishop";

        case PieceType.DarkRook:
        case PieceType.LightRook: return "Rook";

        case PieceType.DarkQueen:
        case PieceType.LightQueen: return "Queen";

        case PieceType.DarkKing:
        case PieceType.LightKing: return "King";

        case PieceType.DarkPegasus:
        case PieceType.LightPegasus: return "Pegasus";

        case PieceType.DarkPyramid:
        case PieceType.LightPyramid: return "Pyramid";

        case PieceType.DarkUnicorn:
        case PieceType.LightUnicorn: return "Unicorn";

        case PieceType.DarkWave:
        case PieceType.LightWave: return "Wave";

        case PieceType.DarkCentaur:
        case PieceType.LightCentaur: return "Centaur";

        case PieceType.DarkSerpent:
        case PieceType.LightSerpent: return "Serpent";

        case PieceType.DarkShaman:
        case PieceType.LightShaman: return "Shaman";

        case PieceType.DarkStarchild:
        case PieceType.LightStarchild: return "Starchild";

        case PieceType.DimStar:
        case PieceType.BrightStar: return "Star";

        case PieceType.Monolith: return "Monolith";

        case PieceType.None: return "";
    }
}

export bool isDark( PieceType pt ) {
    switch ( pt ) {
        case PieceType.DarkStarchild:
        case PieceType.DarkShaman:
        case PieceType.DarkSerpent:
        case PieceType.DarkCentaur:
        case PieceType.DarkWave:
        case PieceType.DarkUnicorn:
        case PieceType.DarkPyramid:
        case PieceType.DarkPegasus:
        case PieceType.DarkKing:
        case PieceType.DarkQueen:
        case PieceType.DarkRook:
        case PieceType.DarkBishop:
        case PieceType.DarkKnight:
        case PieceType.DarkPawn: return true;

        default: return false;
    }
}

export bool isLight( PieceType pt ) {
    switch ( pt ) {
        case PieceType.LightPawn:
        case PieceType.LightKnight:
        case PieceType.LightBishop:
        case PieceType.LightRook:
        case PieceType.LightQueen:
        case PieceType.LightKing:
        case PieceType.LightPegasus:
        case PieceType.LightPyramid:
        case PieceType.LightUnicorn:
        case PieceType.LightWave:
        case PieceType.LightCentaur:
        case PieceType.LightSerpent:
        case PieceType.LightShaman:
        case PieceType.LightStarchild: return true;

        default: return false;
    }
}

export bool isPawn( PieceType pt ) {
    switch ( pt ) {
        case PieceType.LightPawn:
        case PieceType.DarkPawn: return true;

        default: return false;
    }
}

export bool isFigure( PieceType pt ) {
    return ( !isPawn( pt ) );
}

export bool isNone( PieceType pt ) {
    return pt == PieceType.None;
}
