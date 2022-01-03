// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stddef.h>
#include <ctype.h>

#include "cc_piece.h"

/**
    @file cc_piece.c
    @brief Piece related functions.
*/


CcPieceEnum cc_piece_from_symbol( char symbol, bool is_light )
{
    switch ( symbol )
    {
        case ' ' : return CC_PE_None;

        case 'P' : return ( is_light ) ? CC_PE_LightPawn : CC_PE_DarkPawn;
        case 'N' : return ( is_light ) ? CC_PE_LightKnight : CC_PE_DarkKnight;
        case 'B' : return ( is_light ) ? CC_PE_LightBishop : CC_PE_DarkBishop;
        case 'R' : return ( is_light ) ? CC_PE_LightRook : CC_PE_DarkRook;
        case 'Q' : return ( is_light ) ? CC_PE_LightQueen : CC_PE_DarkQueen;
        case 'K' : return ( is_light ) ? CC_PE_LightKing : CC_PE_DarkKing;
        case 'G' : return ( is_light ) ? CC_PE_LightPegasus : CC_PE_DarkPegasus;
        case 'A' : return ( is_light ) ? CC_PE_LightPyramid : CC_PE_DarkPyramid;
        case 'U' : return ( is_light ) ? CC_PE_LightUnicorn : CC_PE_DarkUnicorn;
        case 'W' : return ( is_light ) ? CC_PE_LightWave : CC_PE_DarkWave;
        case 'C' : return ( is_light ) ? CC_PE_LightCentaur : CC_PE_DarkCentaur;
        case 'S' : return ( is_light ) ? CC_PE_LightSerpent : CC_PE_DarkSerpent;
        case 'H' : return ( is_light ) ? CC_PE_LightShaman : CC_PE_DarkShaman;
        case 'I' : return ( is_light ) ? CC_PE_LightStarchild : CC_PE_DarkStarchild;

        case 'T' : return ( is_light ) ? CC_PE_BrightStar : CC_PE_DimStar;

        case 'M' : return CC_PE_Monolith;

        default : return CC_PE_None;
    }
}

bool cc_piece_is_symbol( char c )
{
    switch ( c )
    {
        case 'P' :
        case 'N' :
        case 'B' :
        case 'R' :
        case 'Q' :
        case 'K' :
        case 'G' :
        case 'A' :
        case 'U' :
        case 'W' :
        case 'C' :
        case 'S' :
        case 'H' :
        case 'I' :

        case 'T' :

        case 'M' : return true;

        default : return false;
    }
}

CcPieceEnum cc_piece_opposite( CcPieceEnum pe )
{
    switch ( pe )
    {
        case CC_PE_DimStar : return CC_PE_BrightStar;

        case CC_PE_DarkStarchild : return CC_PE_LightStarchild;
        case CC_PE_DarkShaman : return CC_PE_LightShaman;
        case CC_PE_DarkSerpent : return CC_PE_LightSerpent;
        case CC_PE_DarkCentaur : return CC_PE_LightCentaur;
        case CC_PE_DarkWave : return CC_PE_LightWave;
        case CC_PE_DarkUnicorn : return CC_PE_LightUnicorn;
        case CC_PE_DarkPyramid : return CC_PE_LightPyramid;
        case CC_PE_DarkPegasus : return CC_PE_LightPegasus;
        case CC_PE_DarkKing : return CC_PE_LightKing;
        case CC_PE_DarkQueen : return CC_PE_LightQueen;
        case CC_PE_DarkRook : return CC_PE_LightRook;
        case CC_PE_DarkBishop : return CC_PE_LightBishop;
        case CC_PE_DarkKnight : return CC_PE_LightKnight;
        case CC_PE_DarkPawn : return CC_PE_LightPawn;

        case CC_PE_None : return CC_PE_None;

        case CC_PE_LightPawn : return CC_PE_DarkPawn;
        case CC_PE_LightKnight : return CC_PE_DarkKnight;
        case CC_PE_LightBishop : return CC_PE_DarkBishop;
        case CC_PE_LightRook : return CC_PE_DarkRook;
        case CC_PE_LightQueen : return CC_PE_DarkQueen;
        case CC_PE_LightKing : return CC_PE_DarkPegasus;
        case CC_PE_LightPegasus : return CC_PE_DarkPegasus;
        case CC_PE_LightPyramid : return CC_PE_DarkPyramid;
        case CC_PE_LightUnicorn : return CC_PE_DarkUnicorn;
        case CC_PE_LightWave : return CC_PE_DarkWave;
        case CC_PE_LightCentaur : return CC_PE_DarkCentaur;
        case CC_PE_LightSerpent : return CC_PE_DarkSerpent;
        case CC_PE_LightShaman : return CC_PE_DarkShaman;
        case CC_PE_LightStarchild : return CC_PE_DarkStarchild;

        case CC_PE_BrightStar : return CC_PE_DimStar;

        case CC_PE_Monolith : return CC_PE_Monolith;

        default : return CC_PE_None;
    }
}

char cc_piece_as_char( CcPieceEnum pe )
{
    switch ( pe )
    {
        case CC_PE_DimStar : return 't';

        case CC_PE_DarkStarchild : return 'i';
        case CC_PE_DarkShaman : return 'h';
        case CC_PE_DarkSerpent : return 's';
        case CC_PE_DarkCentaur : return 'c';
        case CC_PE_DarkWave : return 'w';
        case CC_PE_DarkUnicorn : return 'u';
        case CC_PE_DarkPyramid : return 'a';
        case CC_PE_DarkPegasus : return 'g';
        case CC_PE_DarkKing : return 'k';
        case CC_PE_DarkQueen : return 'q';
        case CC_PE_DarkRook : return 'r';
        case CC_PE_DarkBishop : return 'b';
        case CC_PE_DarkKnight : return 'n';
        case CC_PE_DarkPawn : return 'p';

        case CC_PE_None : return ' ';

        case CC_PE_LightPawn : return 'P';
        case CC_PE_LightKnight : return 'N';
        case CC_PE_LightBishop : return 'B';
        case CC_PE_LightRook : return 'R';
        case CC_PE_LightQueen : return 'Q';
        case CC_PE_LightKing : return 'K';
        case CC_PE_LightPegasus : return 'G';
        case CC_PE_LightPyramid : return 'A';
        case CC_PE_LightUnicorn : return 'U';
        case CC_PE_LightWave : return 'W';
        case CC_PE_LightCentaur : return 'C';
        case CC_PE_LightSerpent : return 'S';
        case CC_PE_LightShaman : return 'H';
        case CC_PE_LightStarchild : return 'I';

        case CC_PE_BrightStar : return 'T';

        case CC_PE_Monolith : return 'M';

        default : return '?';
    }
}

char const * cc_piece_label( CcPieceEnum pe )
{
    switch ( pe )
    {
        case CC_PE_DimStar :
        case CC_PE_BrightStar : return "Star";

        case CC_PE_DarkStarchild :
        case CC_PE_LightStarchild : return "Starchild";

        case CC_PE_DarkShaman :
        case CC_PE_LightShaman : return "Shaman";

        case CC_PE_DarkSerpent :
        case CC_PE_LightSerpent : return "Serpent";

        case CC_PE_DarkCentaur :
        case CC_PE_LightCentaur : return "Centaur";

        case CC_PE_DarkWave :
        case CC_PE_LightWave : return "Wave";

        case CC_PE_DarkUnicorn :
        case CC_PE_LightUnicorn : return "Unicorn";

        case CC_PE_DarkPyramid :
        case CC_PE_LightPyramid : return "Pyramid";

        case CC_PE_DarkPegasus :
        case CC_PE_LightPegasus : return "Pegasus";

        case CC_PE_DarkKing :
        case CC_PE_LightKing : return "King";

        case CC_PE_DarkQueen :
        case CC_PE_LightQueen : return "Queen";

        case CC_PE_DarkRook :
        case CC_PE_LightRook : return "Rook";

        case CC_PE_DarkBishop :
        case CC_PE_LightBishop : return "Bishop";

        case CC_PE_DarkKnight :
        case CC_PE_LightKnight : return "Knight";

        case CC_PE_DarkPawn :
        case CC_PE_LightPawn : return "Pawn";

        case CC_PE_None : return "";

        case CC_PE_Monolith : return "Monolith";

        default : return NULL;
    }
}

char cc_piece_symbol( CcPieceEnum pe )
{
    return toupper( cc_piece_as_char( pe ) );
}

CcPieceEnum cc_piece_demoting_to( CcPieceEnum pe )
{
    if ( cc_piece_is_dark( pe, false ) ) return CC_PE_DarkPawn;
    if ( cc_piece_is_light( pe, false ) ) return CC_PE_LightPawn;
    return CC_PE_None;
}

bool cc_piece_is_dark( CcPieceEnum pe, bool include_stars )
{
    switch ( pe )
    {
        case CC_PE_DarkStarchild :
        case CC_PE_DarkShaman :
        case CC_PE_DarkSerpent :
        case CC_PE_DarkCentaur :
        case CC_PE_DarkWave :
        case CC_PE_DarkUnicorn :
        case CC_PE_DarkPyramid :
        case CC_PE_DarkPegasus :
        case CC_PE_DarkKing :
        case CC_PE_DarkQueen :
        case CC_PE_DarkRook :
        case CC_PE_DarkBishop :
        case CC_PE_DarkKnight :
        case CC_PE_DarkPawn :
            return true;

        case CC_PE_DimStar :
            return include_stars;

        default :
            return false;
    }
}

bool cc_piece_is_light( CcPieceEnum pe, bool include_stars )
{
    switch ( pe )
    {
        case CC_PE_LightPawn :
        case CC_PE_LightKnight :
        case CC_PE_LightBishop :
        case CC_PE_LightRook :
        case CC_PE_LightQueen :
        case CC_PE_LightKing :
        case CC_PE_LightPegasus :
        case CC_PE_LightPyramid :
        case CC_PE_LightUnicorn :
        case CC_PE_LightWave :
        case CC_PE_LightCentaur :
        case CC_PE_LightSerpent :
        case CC_PE_LightShaman :
        case CC_PE_LightStarchild :
            return true;

        case CC_PE_BrightStar :
            return include_stars;

        default :
            return false;
    }
}

bool cc_piece_has_same_type( CcPieceEnum pe_1, CcPieceEnum pe_2 )
{
    if ( CC_PIECE_IS_THE_SAME( pe_1, pe_2 ) ) return true;
    if ( pe_1 == cc_piece_opposite( pe_2 ) ) return true;
    return false;
}

bool cc_piece_has_same_color( CcPieceEnum pe_1, CcPieceEnum pe_2 )
{
    if ( cc_piece_is_light( pe_1, false ) && cc_piece_is_light( pe_2, false ) )
        return true;

    if ( cc_piece_is_dark( pe_1, false ) && cc_piece_is_dark( pe_2, false ) )
        return true;

    return false;
}

bool cc_piece_has_same_shade( CcPieceEnum pe_1, CcPieceEnum pe_2 )
{
    if ( ( pe_1 == CC_PE_BrightStar ) && ( pe_2 == CC_PE_BrightStar ) ) return true;
    if ( ( pe_1 == CC_PE_DimStar ) && ( pe_2 == CC_PE_DimStar ) ) return true;
    return false;
}

bool cc_piece_is_opposite( CcPieceEnum pe_1, CcPieceEnum pe_2 )
{
    if ( CC_PIECE_IS_NONE( pe_1 ) || CC_PIECE_IS_NONE( pe_2 ) ) return false;
    if ( CC_PIECE_IS_MONOLITH( pe_1 ) || CC_PIECE_IS_MONOLITH( pe_2 ) ) return false;
    return ( pe_1 == cc_piece_opposite( pe_2 ) );
}

bool cc_piece_has_same_owner( CcPieceEnum pe_1, CcPieceEnum pe_2 )
{
    if ( CC_PIECE_IS_NONE( pe_1 ) || CC_PIECE_IS_NONE( pe_2 ) ) return false;
    if ( CC_PIECE_IS_MONOLITH( pe_1 ) || CC_PIECE_IS_MONOLITH( pe_2 ) ) return false;

    if ( cc_piece_is_light( pe_1, false ) && cc_piece_is_light( pe_2, false ) ) return true;
    if ( cc_piece_is_dark( pe_1, false ) && cc_piece_is_dark( pe_2, false ) ) return true;

    return false;
}

bool cc_piece_has_different_owner( CcPieceEnum pe_1, CcPieceEnum pe_2 )
{
    if ( CC_PIECE_IS_NONE( pe_1 ) || CC_PIECE_IS_NONE( pe_2 ) ) return false;
    if ( CC_PIECE_IS_MONOLITH( pe_1 ) || CC_PIECE_IS_MONOLITH( pe_2 ) ) return false;

    if ( cc_piece_is_light( pe_1, false ) && cc_piece_is_dark( pe_2, false ) ) return true;
    if ( cc_piece_is_dark( pe_1, false ) && cc_piece_is_light( pe_2, false ) ) return true;

    return false;
}

bool cc_piece_is_figure( CcPieceEnum pe,
                         bool include_monolith,
                         bool include_stars )
{
    if ( CC_PIECE_IS_PAWN( pe ) ) return false;
    if ( cc_piece_is_light( pe, include_stars ) ) return true;
    if ( cc_piece_is_dark( pe, include_stars ) ) return true;
    if ( pe == CC_PE_Monolith ) return include_monolith;
    return false;
}
