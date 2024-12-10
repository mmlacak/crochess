// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stddef.h>
#include <ctype.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_piece.h"


CcPieceType cc_piece_from_symbol( char symbol, bool is_light ) {
    switch ( symbol ) {
        case ' ' : return CC_PE_None;

        case 'P' : return ( is_light ) ? CC_PE_LightPawn : CC_PE_DarkPawn;
        case 'N' : return ( is_light ) ? CC_PE_LightKnight : CC_PE_DarkKnight;
        case 'B' : return ( is_light ) ? CC_PE_LightBishop : CC_PE_DarkBishop;
        case 'R' : return ( is_light ) ? CC_PE_LightRook : CC_PE_DarkRook;
        case 'Q' : return ( is_light ) ? CC_PE_LightQueen : CC_PE_DarkQueen;
        case 'K' : return ( is_light ) ? CC_PE_LightKing : CC_PE_DarkKing;
        case 'E' : return ( is_light ) ? CC_PE_LightPegasus : CC_PE_DarkPegasus;
        case 'A' : return ( is_light ) ? CC_PE_LightPyramid : CC_PE_DarkPyramid;
        case 'U' : return ( is_light ) ? CC_PE_LightUnicorn : CC_PE_DarkUnicorn;
        case 'W' : return ( is_light ) ? CC_PE_LightWave : CC_PE_DarkWave;
        case 'C' : return ( is_light ) ? CC_PE_LightCentaur : CC_PE_DarkCentaur;
        case 'O' : return ( is_light ) ? CC_PE_LightScout : CC_PE_DarkScout;
        case 'G' : return ( is_light ) ? CC_PE_LightGrenadier : CC_PE_DarkGrenadier;
        case 'S' : return ( is_light ) ? CC_PE_LightSerpent : CC_PE_DarkSerpent;
        case 'H' : return ( is_light ) ? CC_PE_LightShaman : CC_PE_DarkShaman;
        case 'I' : return ( is_light ) ? CC_PE_LightStarchild : CC_PE_DarkStarchild;

        case 'T' : return ( is_light ) ? CC_PE_BrightStar : CC_PE_DimStar;

        case 'M' : return CC_PE_Monolith;

        default : return CC_PE_None;
    }
}

bool cc_piece_symbol_is_valid( char c ) {
    switch ( c ) {
        case 'P' :
        case 'N' :
        case 'B' :
        case 'R' :
        case 'Q' :
        case 'K' :
        case 'E' :
        case 'A' :
        case 'U' :
        case 'W' :
        case 'C' :
        case 'O' :
        case 'G' :
        case 'S' :
        case 'H' :
        case 'I' :

        case 'T' :

        case 'M' : return true;

        default : return false;
    }
}

CcPieceType cc_piece_opposite( CcPieceType pe ) {
    switch ( pe ) {
        case CC_PE_DimStar : return CC_PE_BrightStar;

        case CC_PE_DarkStarchild : return CC_PE_LightStarchild;
        case CC_PE_DarkShaman : return CC_PE_LightShaman;
        case CC_PE_DarkSerpent : return CC_PE_LightSerpent;
        case CC_PE_DarkGrenadier : return CC_PE_LightGrenadier;
        case CC_PE_DarkScout : return CC_PE_LightScout;
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
        case CC_PE_LightScout : return CC_PE_DarkScout;
        case CC_PE_LightGrenadier : return CC_PE_DarkGrenadier;
        case CC_PE_LightSerpent : return CC_PE_DarkSerpent;
        case CC_PE_LightShaman : return CC_PE_DarkShaman;
        case CC_PE_LightStarchild : return CC_PE_DarkStarchild;

        case CC_PE_BrightStar : return CC_PE_DimStar;

        case CC_PE_Monolith : return CC_PE_Monolith;

        default : return CC_PE_None;
    }
}

char cc_piece_as_char( CcPieceType pe ) {
    switch ( pe ) {
        case CC_PE_DimStar : return 't';

        case CC_PE_DarkStarchild : return 'i';
        case CC_PE_DarkShaman : return 'h';
        case CC_PE_DarkSerpent : return 's';
        case CC_PE_DarkGrenadier : return 'g';
        case CC_PE_DarkScout : return 'o';
        case CC_PE_DarkCentaur : return 'c';
        case CC_PE_DarkWave : return 'w';
        case CC_PE_DarkUnicorn : return 'u';
        case CC_PE_DarkPyramid : return 'a';
        case CC_PE_DarkPegasus : return 'e';
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
        case CC_PE_LightPegasus : return 'E';
        case CC_PE_LightPyramid : return 'A';
        case CC_PE_LightUnicorn : return 'U';
        case CC_PE_LightWave : return 'W';
        case CC_PE_LightCentaur : return 'C';
        case CC_PE_LightScout : return 'O';
        case CC_PE_LightGrenadier : return 'G';
        case CC_PE_LightSerpent : return 'S';
        case CC_PE_LightShaman : return 'H';
        case CC_PE_LightStarchild : return 'I';

        case CC_PE_BrightStar : return 'T';

        case CC_PE_Monolith : return 'M';

        default : return '?';
    }
}

CcPieceType cc_piece_from_char( char piece ) {
    switch ( piece ) {
        case 't' : return CC_PE_DimStar;

        case 'i' : return CC_PE_DarkStarchild;
        case 'h' : return CC_PE_DarkShaman;
        case 's' : return CC_PE_DarkSerpent;
        case 'g' : return CC_PE_DarkGrenadier;
        case 'o' : return CC_PE_DarkScout;
        case 'c' : return CC_PE_DarkCentaur;
        case 'w' : return CC_PE_DarkWave;
        case 'u' : return CC_PE_DarkUnicorn;
        case 'a' : return CC_PE_DarkPyramid;
        case 'e' : return CC_PE_DarkPegasus;
        case 'k' : return CC_PE_DarkKing;
        case 'q' : return CC_PE_DarkQueen;
        case 'r' : return CC_PE_DarkRook;
        case 'b' : return CC_PE_DarkBishop;
        case 'n' : return CC_PE_DarkKnight;
        case 'p' : return CC_PE_DarkPawn;

        case ' ' : return CC_PE_None;

        case 'P' : return CC_PE_LightPawn;
        case 'N' : return CC_PE_LightKnight;
        case 'B' : return CC_PE_LightBishop;
        case 'R' : return CC_PE_LightRook;
        case 'Q' : return CC_PE_LightQueen;
        case 'K' : return CC_PE_LightKing;
        case 'E' : return CC_PE_LightPegasus;
        case 'A' : return CC_PE_LightPyramid;
        case 'U' : return CC_PE_LightUnicorn;
        case 'W' : return CC_PE_LightWave;
        case 'C' : return CC_PE_LightCentaur;
        case 'O' : return CC_PE_LightScout;
        case 'G' : return CC_PE_LightGrenadier;
        case 'S' : return CC_PE_LightSerpent;
        case 'H' : return CC_PE_LightShaman;
        case 'I' : return CC_PE_LightStarchild;

        case 'T' : return CC_PE_BrightStar;

        case 'M' : return CC_PE_Monolith;

        default : return CC_PE_None;
    }
}

char const * cc_piece_label( CcPieceType pe, bool capitalize, bool empty_field ) {
    switch ( pe ) {
        case CC_PE_DimStar :
        case CC_PE_BrightStar : return "Star";

        case CC_PE_DarkStarchild :
        case CC_PE_LightStarchild : return "Starchild";

        case CC_PE_DarkShaman :
        case CC_PE_LightShaman : return "Shaman";

        case CC_PE_DarkSerpent :
        case CC_PE_LightSerpent : return "Serpent";

        case CC_PE_DarkGrenadier :
        case CC_PE_LightGrenadier : return "Grenadier";

        case CC_PE_DarkScout :
        case CC_PE_LightScout : return "Scout";

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

        case CC_PE_None :
            return empty_field ? ( capitalize ? "Empty field"
                                              : "empty field" )
                               : "";

        case CC_PE_Monolith : return "Monolith";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}

char cc_piece_symbol( CcPieceType pe ) {
    return toupper( cc_piece_as_char( pe ) );
}

CcPieceType cc_piece_demoting_to( CcPieceType pe ) {
    if ( cc_piece_is_dark( pe ) ) return CC_PE_DarkPawn;
    if ( cc_piece_is_light( pe ) ) return CC_PE_LightPawn;
    return CC_PE_None;
}

bool cc_piece_is_dark( CcPieceType pe ) {
    switch ( pe ) {
        case CC_PE_DarkStarchild :
        case CC_PE_DarkShaman :
        case CC_PE_DarkSerpent :
        case CC_PE_DarkGrenadier :
        case CC_PE_DarkScout :
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

        default :
            return false;
    }
}

bool cc_piece_is_light( CcPieceType pe ) {
    switch ( pe ) {
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
        case CC_PE_LightScout :
        case CC_PE_LightGrenadier :
        case CC_PE_LightSerpent :
        case CC_PE_LightShaman :
        case CC_PE_LightStarchild :
            return true;

        default :
            return false;
    }
}

bool cc_piece_has_color( CcPieceType pe ) {
    return cc_piece_is_light( pe ) || cc_piece_is_dark( pe );
}

bool cc_piece_has_shade( CcPieceType pe ) {
    return CC_PIECE_IS_STAR( pe );
}

bool cc_piece_has_prefix( CcPieceType pe ) {
    return cc_piece_has_color( pe ) || cc_piece_has_shade( pe );
}

char const * cc_piece_prefix( CcPieceType pe, bool capitalize ) {
    switch ( pe ) {
        case CC_PE_DimStar : return capitalize ? "Dim" : "dim";

        case CC_PE_DarkStarchild :
        case CC_PE_DarkShaman :
        case CC_PE_DarkSerpent :
        case CC_PE_DarkGrenadier :
        case CC_PE_DarkScout :
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
        case CC_PE_DarkPawn : return capitalize ? "Dark" : "dark";

        case CC_PE_None : return "";

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
        case CC_PE_LightScout :
        case CC_PE_LightGrenadier :
        case CC_PE_LightSerpent :
        case CC_PE_LightShaman :
        case CC_PE_LightStarchild : return capitalize ? "Light" : "light";

        case CC_PE_BrightStar : return capitalize ? "Bright" : "bright";

        case CC_PE_Monolith : return "";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}

bool cc_piece_has_congruent_type( char symbol, CcPieceType pe ) {
    char ps = cc_piece_symbol( pe );
    return ( symbol == ps );
}

bool cc_piece_is_equal( char symbol, bool is_light, CcPieceType pe ) {
    CcPieceType piece = cc_piece_from_symbol( symbol, is_light );
    return ( piece == pe );
}

bool cc_piece_has_same_type( CcPieceType pe_1, CcPieceType pe_2 ) {
    if ( pe_1 == pe_2 ) return true;
    if ( pe_1 == cc_piece_opposite( pe_2 ) ) return true;
    return false;
}

bool cc_piece_has_same_color( CcPieceType pe_1, CcPieceType pe_2 ) {
    if ( cc_piece_is_light( pe_1 ) && cc_piece_is_light( pe_2 ) )
        return true;

    if ( cc_piece_is_dark( pe_1 ) && cc_piece_is_dark( pe_2 ) )
        return true;

    return false;
}

bool cc_piece_has_same_shade( CcPieceType pe_1, CcPieceType pe_2 ) {
    if ( ( pe_1 == CC_PE_BrightStar ) && ( pe_2 == CC_PE_BrightStar ) ) return true;
    if ( ( pe_1 == CC_PE_DimStar ) && ( pe_2 == CC_PE_DimStar ) ) return true;
    return false;
}

bool cc_piece_is_opposite( CcPieceType pe_1, CcPieceType pe_2 ) {
    if ( ( !CC_PIECE_HAS_OWNER( pe_1 ) ) || ( !CC_PIECE_HAS_OWNER( pe_2 ) ) ) return false;

    return ( pe_1 == cc_piece_opposite( pe_2 ) );
}

bool cc_piece_has_same_owner( CcPieceType pe_1, CcPieceType pe_2 ) {
    if ( cc_piece_is_light( pe_1 ) && cc_piece_is_light( pe_2 ) ) return true;
    if ( cc_piece_is_dark( pe_1 ) && cc_piece_is_dark( pe_2 ) ) return true;

    return false;
}

bool cc_piece_has_different_owner( CcPieceType pe_1, CcPieceType pe_2 ) {
    if ( cc_piece_is_light( pe_1 ) && cc_piece_is_dark( pe_2 ) ) return true;
    if ( cc_piece_is_dark( pe_1 ) && cc_piece_is_light( pe_2 ) ) return true;

    return false;
}

bool cc_piece_is_owned_figure( CcPieceType pe ) {
    if ( CC_PIECE_IS_PAWN( pe ) ) return false;
    if ( cc_piece_is_light( pe ) ) return true;
    if ( cc_piece_is_dark( pe ) ) return true;
    return false;
}

bool cc_piece_is_figure( CcPieceType pe ) {
    if ( CC_PIECE_IS_STAR( pe ) ) return true;
    if ( CC_PIECE_IS_MONOLITH( pe ) ) return true;

    return cc_piece_is_owned_figure( pe );
}

char const * cc_piece_as_string( CcPieceType pe, bool capitalize, bool empty_field ) {
    switch ( pe ) {
        case CC_PE_DimStar : return capitalize ? "Dim Star" : "dim Star";
        case CC_PE_BrightStar : return capitalize ? "Bright Star" : "bright Star";

        case CC_PE_DarkStarchild : return capitalize ? "Dark Starchild" : "dark Starchild";
        case CC_PE_LightStarchild : return capitalize ? "Light Starchild" : "light Starchild";

        case CC_PE_DarkShaman : return capitalize ? "Dark Shaman" : "dark Shaman";
        case CC_PE_LightShaman : return capitalize ? "Light Shaman" : "light Shaman";

        case CC_PE_DarkSerpent : return capitalize ? "Dark Serpent" : "dark Serpent";
        case CC_PE_LightSerpent : return capitalize ? "Light Serpent" : "light Serpent";

        case CC_PE_DarkGrenadier : return capitalize ? "Dark Grenadier" : "dark Grenadier";
        case CC_PE_LightGrenadier : return capitalize ? "Light Grenadier" : "light Grenadier";

        case CC_PE_DarkScout : return capitalize ? "Dark Scout" : "dark Scout";
        case CC_PE_LightScout : return capitalize ? "Light Scout" : "light Scout";

        case CC_PE_DarkCentaur : return capitalize ? "Dark Centaur" : "dark Centaur";
        case CC_PE_LightCentaur : return capitalize ? "Light Centaur" : "light Centaur";

        case CC_PE_DarkWave : return capitalize ? "Dark Wave" : "dark Wave";
        case CC_PE_LightWave : return capitalize ? "Light Wave" : "light Wave";

        case CC_PE_DarkUnicorn : return capitalize ? "Dark Unicorn" : "dark Unicorn";
        case CC_PE_LightUnicorn : return capitalize ? "Light Unicorn" : "light Unicorn";

        case CC_PE_DarkPyramid : return capitalize ? "Dark Pyramid" : "dark Pyramid";
        case CC_PE_LightPyramid : return capitalize ? "Light Pyramid" : "light Pyramid";

        case CC_PE_DarkPegasus : return capitalize ? "Dark Pegasus" : "dark Pegasus";
        case CC_PE_LightPegasus : return capitalize ? "Light Pegasus" : "light Pegasus";

        case CC_PE_DarkKing : return capitalize ? "Dark King" : "dark King";
        case CC_PE_LightKing : return capitalize ? "Light King" : "light King";

        case CC_PE_DarkQueen : return capitalize ? "Dark Queen" : "dark Queen";
        case CC_PE_LightQueen : return capitalize ? "Light Queen" : "light Queen";

        case CC_PE_DarkRook : return capitalize ? "Dark Rook" : "dark Rook";
        case CC_PE_LightRook : return capitalize ? "Light Rook" : "light Rook";

        case CC_PE_DarkBishop : return capitalize ? "Dark Bishop" : "dark Bishop";
        case CC_PE_LightBishop : return capitalize ? "Light Bishop" : "light Bishop";

        case CC_PE_DarkKnight : return capitalize ? "Dark Knight" : "dark Knight";
        case CC_PE_LightKnight : return capitalize ? "Light Knight" : "light Knight";

        case CC_PE_DarkPawn : return capitalize ? "Dark Pawn" : "dark Pawn";
        case CC_PE_LightPawn : return capitalize ? "Light Pawn" : "light Pawn";

        case CC_PE_None :
            return empty_field ? ( capitalize ? "Empty field"
                                              : "empty field" )
                               : "";

        case CC_PE_Monolith : return "Monolith";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}
