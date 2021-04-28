// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <ctype.h>

#include "piece_type.h"

PieceType pt_from_symbol(char const c, bool const is_light)
{
    switch ( c )
    {
        case ' ' : return PT_None;

        case 'P' : return ( is_light ) ? PT_LightPawn : PT_DarkPawn;
        case 'N' : return ( is_light ) ? PT_LightKnight : PT_DarkKnight;
        case 'B' : return ( is_light ) ? PT_LightBishop : PT_DarkBishop;
        case 'R' : return ( is_light ) ? PT_LightRook : PT_DarkRook;
        case 'Q' : return ( is_light ) ? PT_LightQueen : PT_DarkQueen;
        case 'K' : return ( is_light ) ? PT_LightKing : PT_DarkKing;
        case 'G' : return ( is_light ) ? PT_LightPegasus : PT_DarkPegasus;
        case 'A' : return ( is_light ) ? PT_LightPyramid : PT_DarkPyramid;
        case 'U' : return ( is_light ) ? PT_LightUnicorn : PT_DarkUnicorn;
        case 'W' : return ( is_light ) ? PT_LightWave : PT_DarkWave;
        case 'C' : return ( is_light ) ? PT_LightCentaur : PT_DarkCentaur;
        case 'S' : return ( is_light ) ? PT_LightSerpent : PT_DarkSerpent;
        case 'H' : return ( is_light ) ? PT_LightShaman : PT_DarkShaman;
        case 'I' : return ( is_light ) ? PT_LightStarchild : PT_DarkStarchild;

        case 'T' : return ( is_light ) ? PT_BrightStar : PT_DimStar;

        case 'M' : return PT_Monolith;

        default : return PT_None;
    }
}

PieceType pt_opposite(PieceType const pt)
{
    switch ( pt )
    {
        case PT_DimStar : return PT_BrightStar;

        case PT_DarkStarchild : return PT_LightStarchild;
        case PT_DarkShaman : return PT_LightShaman;
        case PT_DarkSerpent : return PT_LightSerpent;
        case PT_DarkCentaur : return PT_LightCentaur;
        case PT_DarkWave : return PT_LightWave;
        case PT_DarkUnicorn : return PT_LightUnicorn;
        case PT_DarkPyramid : return PT_LightPyramid;
        case PT_DarkPegasus : return PT_LightPegasus;
        case PT_DarkKing : return PT_LightKing;
        case PT_DarkQueen : return PT_LightQueen;
        case PT_DarkRook : return PT_LightRook;
        case PT_DarkBishop : return PT_LightBishop;
        case PT_DarkKnight : return PT_LightKnight;
        case PT_DarkPawn : return PT_LightPawn;

        case PT_None : return PT_None;

        case PT_LightPawn : return PT_DarkPawn;
        case PT_LightKnight : return PT_DarkKnight;
        case PT_LightBishop : return PT_DarkBishop;
        case PT_LightRook : return PT_DarkRook;
        case PT_LightQueen : return PT_DarkQueen;
        case PT_LightKing : return PT_DarkPegasus;
        case PT_LightPegasus : return PT_DarkPegasus;
        case PT_LightPyramid : return PT_DarkPyramid;
        case PT_LightUnicorn : return PT_DarkUnicorn;
        case PT_LightWave : return PT_DarkWave;
        case PT_LightCentaur : return PT_DarkCentaur;
        case PT_LightSerpent : return PT_DarkSerpent;
        case PT_LightShaman : return PT_DarkShaman;
        case PT_LightStarchild : return PT_DarkStarchild;

        case PT_BrightStar : return PT_DimStar;

        case PT_Monolith : return PT_Monolith;

        default : return PT_None;
    }
}

char pt_as_char(PieceType const pt)
{
    switch ( pt )
    {
        case PT_DimStar : return 't';

        case PT_DarkStarchild : return 'i';
        case PT_DarkShaman : return 'h';
        case PT_DarkSerpent : return 's';
        case PT_DarkCentaur : return 'c';
        case PT_DarkWave : return 'w';
        case PT_DarkUnicorn : return 'u';
        case PT_DarkPyramid : return 'a';
        case PT_DarkPegasus : return 'g';
        case PT_DarkKing : return 'k';
        case PT_DarkQueen : return 'q';
        case PT_DarkRook : return 'r';
        case PT_DarkBishop : return 'b';
        case PT_DarkKnight : return 'n';
        case PT_DarkPawn : return 'p';

        case PT_None : return ' ';

        case PT_LightPawn : return 'P';
        case PT_LightKnight : return 'N';
        case PT_LightBishop : return 'B';
        case PT_LightRook : return 'R';
        case PT_LightQueen : return 'Q';
        case PT_LightKing : return 'K';
        case PT_LightPegasus : return 'G';
        case PT_LightPyramid : return 'A';
        case PT_LightUnicorn : return 'U';
        case PT_LightWave : return 'W';
        case PT_LightCentaur : return 'C';
        case PT_LightSerpent : return 'S';
        case PT_LightShaman : return 'H';
        case PT_LightStarchild : return 'I';

        case PT_BrightStar : return 'T';

        case PT_Monolith : return 'M';

        default : return ' ';
    }
}

char pt_symbol(PieceType const pt)
{
    return toupper( pt_as_char( pt ) );
}

char const * pt_label(PieceType const pt)
{
    switch ( pt )
    {
        case PT_DimStar :
        case PT_BrightStar : return "Star";

        case PT_DarkStarchild :
        case PT_LightStarchild : return "Starchild";

        case PT_DarkShaman :
        case PT_LightShaman : return "Shaman";

        case PT_DarkSerpent :
        case PT_LightSerpent : return "Serpent";

        case PT_DarkCentaur :
        case PT_LightCentaur : return "Centaur";

        case PT_DarkWave :
        case PT_LightWave : return "Wave";

        case PT_DarkUnicorn :
        case PT_LightUnicorn : return "Unicorn";

        case PT_DarkPyramid :
        case PT_LightPyramid : return "Pyramid";

        case PT_DarkPegasus :
        case PT_LightPegasus : return "Pegasus";

        case PT_DarkKing :
        case PT_LightKing : return "King";

        case PT_DarkQueen :
        case PT_LightQueen : return "Queen";

        case PT_DarkRook :
        case PT_LightRook : return "Rook";

        case PT_DarkBishop :
        case PT_LightBishop : return "Bishop";

        case PT_DarkKnight :
        case PT_LightKnight : return "Knight";

        case PT_DarkPawn :
        case PT_LightPawn : return "Pawn";

        case PT_None : return "";

        case PT_Monolith : return "Monolith";

        default : return "";
    }
}

bool pt_is_dark(PieceType const pt)
{
    switch ( pt )
    {
        case PT_DarkStarchild :
        case PT_DarkShaman :
        case PT_DarkSerpent :
        case PT_DarkCentaur :
        case PT_DarkWave :
        case PT_DarkUnicorn :
        case PT_DarkPyramid :
        case PT_DarkPegasus :
        case PT_DarkKing :
        case PT_DarkQueen :
        case PT_DarkRook :
        case PT_DarkBishop :
        case PT_DarkKnight :
        case PT_DarkPawn : return true;

        default : return false;
    }
}

bool pt_is_light(PieceType const pt)
{
    switch ( pt )
    {
        case PT_LightPawn :
        case PT_LightKnight :
        case PT_LightBishop :
        case PT_LightRook :
        case PT_LightQueen :
        case PT_LightKing :
        case PT_LightPegasus :
        case PT_LightPyramid :
        case PT_LightUnicorn :
        case PT_LightWave :
        case PT_LightCentaur :
        case PT_LightSerpent :
        case PT_LightShaman :
        case PT_LightStarchild : return true;

        default : return false;
    }
}

bool pt_is_pawn(PieceType const pt)
{
    return ( ( pt == PT_LightPawn ) || ( pt == PT_DarkPawn ) );
}

bool pt_is_figure(PieceType const pt)
{
    if ( pt_is_light(pt) ) return true;
    if ( pt_is_dark(pt) ) return true;
    return false;
}

bool pt_is_none(PieceType const pt)
{
    return ( pt == PT_None );
}


bool pt_is_opposite_color(PieceType const pt1, PieceType const pt2)
{
    if ( pt_is_light( pt1 ) && pt_is_dark( pt2 ) ) return true;
    if ( pt_is_dark( pt1 ) && pt_is_light( pt2 ) ) return true;

    return false;
}

bool pt_is_opposite_shade(PieceType const pt1, PieceType const pt2)
{
    if ( ( ( pt1 == PT_DimStar ) && ( pt2 == PT_BrightStar ) )
        || ( ( pt1 == PT_BrightStar ) && ( pt2 == PT_DimStar ) ) ) return true;

    return false;
}

bool pt_is_teleporter(PieceType const pt)
{
    return ( ( pt == PT_Monolith ) || ( pt == PT_DimStar ) || ( pt == PT_BrightStar ) );
}
