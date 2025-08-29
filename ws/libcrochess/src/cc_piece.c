// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stddef.h>
#include <ctype.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_piece.h"
#include "cc_tag.h"


CcPieceTagType cc_piece_from_symbol( char symbol, bool is_light ) {
    switch ( symbol ) {
        case ' ' : return CC_PTE_None;

        case 'P' : return ( is_light ) ? CC_PTE_LightPawn : CC_PTE_DarkPawn;
        case 'N' : return ( is_light ) ? CC_PTE_LightKnight : CC_PTE_DarkKnight;
        case 'B' : return ( is_light ) ? CC_PTE_LightBishop : CC_PTE_DarkBishop;
        case 'R' : return ( is_light ) ? CC_PTE_LightRook : CC_PTE_DarkRook;
        case 'Q' : return ( is_light ) ? CC_PTE_LightQueen : CC_PTE_DarkQueen;
        case 'K' : return ( is_light ) ? CC_PTE_LightKing : CC_PTE_DarkKing;
        case 'E' : return ( is_light ) ? CC_PTE_LightPegasus : CC_PTE_DarkPegasus;
        case 'A' : return ( is_light ) ? CC_PTE_LightPyramid : CC_PTE_DarkPyramid;
        case 'U' : return ( is_light ) ? CC_PTE_LightUnicorn : CC_PTE_DarkUnicorn;
        case 'W' : return ( is_light ) ? CC_PTE_LightWave : CC_PTE_DarkWave;
        case 'C' : return ( is_light ) ? CC_PTE_LightCentaur : CC_PTE_DarkCentaur;
        case 'O' : return ( is_light ) ? CC_PTE_LightScout : CC_PTE_DarkScout;
        case 'G' : return ( is_light ) ? CC_PTE_LightGrenadier : CC_PTE_DarkGrenadier;
        case 'S' : return ( is_light ) ? CC_PTE_LightSerpent : CC_PTE_DarkSerpent;
        case 'H' : return ( is_light ) ? CC_PTE_LightShaman : CC_PTE_DarkShaman;
        case 'I' : return ( is_light ) ? CC_PTE_LightStarchild : CC_PTE_DarkStarchild;

        case 'T' : return ( is_light ) ? CC_PTE_BrightStar : CC_PTE_DimStar;

        case 'M' : return CC_PTE_Monolith;

        default : return CC_PTE_None;
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

CcPieceTagType cc_piece_strip_tag( CcPieceTagType ptt ) {
    switch ( ptt ) {
        case CC_PTE_DarkGrenadier_RushedCurrent :
        case CC_PTE_DarkGrenadier_RushedPrevious :
        case CC_PTE_DarkGrenadier_CanRush :
        case CC_PTE_DarkGrenadier : return CC_PTE_DarkGrenadier;

        case CC_PTE_DarkScout_RushedCurrent :
        case CC_PTE_DarkScout_RushedPrevious :
        case CC_PTE_DarkScout_CanRush :
        case CC_PTE_DarkScout : return CC_PTE_DarkScout;

        case CC_PTE_DarkKing_CanCastle :
        case CC_PTE_DarkKing : return CC_PTE_DarkKing;

        case CC_PTE_DarkRook_CanCastle :
        case CC_PTE_DarkRook : return CC_PTE_DarkRook;

        case CC_PTE_DarkPawn_DelayedPromotion :
        case CC_PTE_DarkPawn_RushedCurrent :
        case CC_PTE_DarkPawn_RushedPrevious :
        case CC_PTE_DarkPawn_CanRush :
        case CC_PTE_DarkPawn : return CC_PTE_DarkPawn;

        case CC_PTE_LightPawn :
        case CC_PTE_LightPawn_CanRush :
        case CC_PTE_LightPawn_RushedPrevious :
        case CC_PTE_LightPawn_RushedCurrent :
        case CC_PTE_LightPawn_DelayedPromotion : return CC_PTE_LightPawn;

        case CC_PTE_LightRook :
        case CC_PTE_LightRook_CanCastle : return CC_PTE_LightRook;

        case CC_PTE_LightKing :
        case CC_PTE_LightKing_CanCastle : return CC_PTE_LightKing;

        case CC_PTE_LightScout :
        case CC_PTE_LightScout_CanRush :
        case CC_PTE_LightScout_RushedPrevious :
        case CC_PTE_LightScout_RushedCurrent : return CC_PTE_LightScout;

        case CC_PTE_LightGrenadier :
        case CC_PTE_LightGrenadier_CanRush :
        case CC_PTE_LightGrenadier_RushedPrevious :
        case CC_PTE_LightGrenadier_RushedCurrent : return CC_PTE_LightGrenadier;

        case CC_PTE_DimStar : // Intentional fall-through.

        case CC_PTE_DarkStarchild :
        case CC_PTE_DarkShaman :
        case CC_PTE_DarkSerpent :

        case CC_PTE_DarkCentaur :
        case CC_PTE_DarkWave :
        case CC_PTE_DarkUnicorn :
        case CC_PTE_DarkPyramid :
        case CC_PTE_DarkPegasus :

        case CC_PTE_DarkQueen :

        case CC_PTE_DarkBishop :
        case CC_PTE_DarkKnight :

        case CC_PTE_None :

        case CC_PTE_LightKnight :
        case CC_PTE_LightBishop :

        case CC_PTE_LightQueen :

        case CC_PTE_LightPegasus :
        case CC_PTE_LightPyramid :
        case CC_PTE_LightUnicorn :
        case CC_PTE_LightWave :
        case CC_PTE_LightCentaur :

        case CC_PTE_LightSerpent :
        case CC_PTE_LightShaman :
        case CC_PTE_LightStarchild :

        case CC_PTE_BrightStar :

        case CC_PTE_Monolith : return ptt;

        default : return CC_PTE_None;
    }
}

CcPieceTagType cc_piece_opposite( CcPieceTagType ptt ) {
    if ( CC_PIECE_HAS_OPPOSITE( ptt ) ) // No need to check validity here, CC_PIECE_HAS_OPPOSITE() just excludes Monolith, in addition to None.
        return ( -ptt );
    else if ( ptt == CC_PTE_Monolith )
        return CC_PTE_Monolith;
    else
        return CC_PTE_None;
}

char cc_piece_as_char( CcPieceTagType ptt ) {
    switch ( ptt ) {
        case CC_PTE_DimStar : return 't';

        case CC_PTE_DarkStarchild : return 'i';
        case CC_PTE_DarkShaman : return 'h';
        case CC_PTE_DarkSerpent : return 's';

        case CC_PTE_DarkGrenadier_RushedCurrent :
        case CC_PTE_DarkGrenadier_RushedPrevious :
        case CC_PTE_DarkGrenadier_CanRush :
        case CC_PTE_DarkGrenadier : return 'g';

        case CC_PTE_DarkScout_RushedCurrent :
        case CC_PTE_DarkScout_RushedPrevious :
        case CC_PTE_DarkScout_CanRush :
        case CC_PTE_DarkScout : return 'o';

        case CC_PTE_DarkCentaur : return 'c';
        case CC_PTE_DarkWave : return 'w';
        case CC_PTE_DarkUnicorn : return 'u';
        case CC_PTE_DarkPyramid : return 'a';
        case CC_PTE_DarkPegasus : return 'e';

        case CC_PTE_DarkKing_CanCastle :
        case CC_PTE_DarkKing : return 'k';

        case CC_PTE_DarkQueen : return 'q';

        case CC_PTE_DarkRook_CanCastle :
        case CC_PTE_DarkRook : return 'r';

        case CC_PTE_DarkBishop : return 'b';
        case CC_PTE_DarkKnight : return 'n';

        case CC_PTE_DarkPawn_DelayedPromotion :
        case CC_PTE_DarkPawn_RushedCurrent :
        case CC_PTE_DarkPawn_RushedPrevious :
        case CC_PTE_DarkPawn_CanRush :
        case CC_PTE_DarkPawn : return 'p';

        case CC_PTE_None : return ' ';

        case CC_PTE_LightPawn :
        case CC_PTE_LightPawn_CanRush :
        case CC_PTE_LightPawn_RushedPrevious :
        case CC_PTE_LightPawn_RushedCurrent :
        case CC_PTE_LightPawn_DelayedPromotion : return 'P';

        case CC_PTE_LightKnight : return 'N';
        case CC_PTE_LightBishop : return 'B';

        case CC_PTE_LightRook :
        case CC_PTE_LightRook_CanCastle : return 'R';

        case CC_PTE_LightQueen : return 'Q';

        case CC_PTE_LightKing :
        case CC_PTE_LightKing_CanCastle : return 'K';

        case CC_PTE_LightPegasus : return 'E';
        case CC_PTE_LightPyramid : return 'A';
        case CC_PTE_LightUnicorn : return 'U';
        case CC_PTE_LightWave : return 'W';
        case CC_PTE_LightCentaur : return 'C';

        case CC_PTE_LightScout :
        case CC_PTE_LightScout_CanRush :
        case CC_PTE_LightScout_RushedPrevious :
        case CC_PTE_LightScout_RushedCurrent : return 'O';

        case CC_PTE_LightGrenadier :
        case CC_PTE_LightGrenadier_CanRush :
        case CC_PTE_LightGrenadier_RushedPrevious :
        case CC_PTE_LightGrenadier_RushedCurrent : return 'G';

        case CC_PTE_LightSerpent : return 'S';
        case CC_PTE_LightShaman : return 'H';
        case CC_PTE_LightStarchild : return 'I';

        case CC_PTE_BrightStar : return 'T';

        case CC_PTE_Monolith : return 'M';

        default : return '?';
    }
}

CcPieceTagType cc_piece_from_char( char piece, char tag ) {
    switch ( piece ) {
        case 't' : return CC_PTE_DimStar;

        case 'i' : return CC_PTE_DarkStarchild;
        case 'h' : return CC_PTE_DarkShaman;
        case 's' : return CC_PTE_DarkSerpent;

        case 'g' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_RUSH : return CC_PTE_DarkGrenadier_CanRush;
                case CC_TAG_CHAR_RUSHED_PREVIOUS : return CC_PTE_DarkGrenadier_RushedPrevious;
                case CC_TAG_CHAR_RUSHED_CURRENT : return CC_PTE_DarkGrenadier_RushedCurrent;
                default : return CC_PTE_DarkGrenadier;
            }
        }

        case 'o' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_RUSH : return CC_PTE_DarkScout_CanRush;
                case CC_TAG_CHAR_RUSHED_PREVIOUS : return CC_PTE_DarkScout_RushedPrevious;
                case CC_TAG_CHAR_RUSHED_CURRENT : return CC_PTE_DarkScout_RushedCurrent;
                default : return CC_PTE_DarkScout;
            }
        }

        case 'c' : return CC_PTE_DarkCentaur;
        case 'w' : return CC_PTE_DarkWave;
        case 'u' : return CC_PTE_DarkUnicorn;
        case 'a' : return CC_PTE_DarkPyramid;
        case 'e' : return CC_PTE_DarkPegasus;

        case 'k' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_CASTLE : return CC_PTE_DarkKing_CanCastle;
                default : return CC_PTE_DarkKing;
            }
        }

        case 'q' : return CC_PTE_DarkQueen;

        case 'r' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_CASTLE : return CC_PTE_DarkRook_CanCastle;
                default : return CC_PTE_DarkRook;
            }
        }

        case 'b' : return CC_PTE_DarkBishop;
        case 'n' : return CC_PTE_DarkKnight;

        case 'p' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_RUSH : return CC_PTE_DarkPawn_CanRush;
                case CC_TAG_CHAR_DELAYED_PROMOTION : return CC_PTE_DarkPawn_DelayedPromotion;
                case CC_TAG_CHAR_RUSHED_PREVIOUS : return CC_PTE_DarkPawn_RushedPrevious;
                case CC_TAG_CHAR_RUSHED_CURRENT : return CC_PTE_DarkPawn_RushedCurrent;
                default : return CC_PTE_DarkPawn;
            }
        }

        case ' ' : return CC_PTE_None;

        case 'P' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_RUSH : return CC_PTE_LightPawn_CanRush;
                case CC_TAG_CHAR_DELAYED_PROMOTION : return CC_PTE_LightPawn_DelayedPromotion;
                case CC_TAG_CHAR_RUSHED_PREVIOUS : return CC_PTE_LightPawn_RushedPrevious;
                case CC_TAG_CHAR_RUSHED_CURRENT : return CC_PTE_LightPawn_RushedCurrent;
                default : return CC_PTE_LightPawn;
            }
        }

        case 'N' : return CC_PTE_LightKnight;
        case 'B' : return CC_PTE_LightBishop;

        case 'R' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_CASTLE : return CC_PTE_LightRook_CanCastle;
                default : return CC_PTE_LightRook;
            }
        }

        case 'Q' : return CC_PTE_LightQueen;

        case 'K' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_CASTLE : return CC_PTE_LightKing_CanCastle;
                default : return CC_PTE_LightKing;
            }
        }

        case 'E' : return CC_PTE_LightPegasus;
        case 'A' : return CC_PTE_LightPyramid;
        case 'U' : return CC_PTE_LightUnicorn;
        case 'W' : return CC_PTE_LightWave;
        case 'C' : return CC_PTE_LightCentaur;

        case 'O' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_RUSH : return CC_PTE_LightScout_CanRush;
                case CC_TAG_CHAR_RUSHED_PREVIOUS : return CC_PTE_LightScout_RushedPrevious;
                case CC_TAG_CHAR_RUSHED_CURRENT : return CC_PTE_LightScout_RushedCurrent;
                default : return CC_PTE_LightScout;
            }
        }

        case 'G' : {
            switch ( tag ) {
                case CC_TAG_CHAR_CAN_RUSH : return CC_PTE_LightGrenadier_CanRush;
                case CC_TAG_CHAR_RUSHED_PREVIOUS : return CC_PTE_LightGrenadier_RushedPrevious;
                case CC_TAG_CHAR_RUSHED_CURRENT : return CC_PTE_LightGrenadier_RushedCurrent;
                default : return CC_PTE_LightGrenadier;
            }
        }

        case 'S' : return CC_PTE_LightSerpent;
        case 'H' : return CC_PTE_LightShaman;
        case 'I' : return CC_PTE_LightStarchild;

        case 'T' : return CC_PTE_BrightStar;

        case 'M' : return CC_PTE_Monolith;

        default : return CC_PTE_None;
    }
}

char const * cc_piece_label( CcPieceTagType ptt, bool capitalize, bool empty_field ) {
    switch ( ptt ) {
        case CC_PTE_DimStar :
        case CC_PTE_BrightStar : return "Star";

        case CC_PTE_DarkStarchild :
        case CC_PTE_LightStarchild : return "Starchild";

        case CC_PTE_DarkShaman :
        case CC_PTE_LightShaman : return "Shaman";

        case CC_PTE_DarkSerpent :
        case CC_PTE_LightSerpent : return "Serpent";

        case CC_PTE_DarkGrenadier_RushedCurrent :
        case CC_PTE_DarkGrenadier_RushedPrevious :
        case CC_PTE_DarkGrenadier_CanRush :
        case CC_PTE_DarkGrenadier :
        case CC_PTE_LightGrenadier :
        case CC_PTE_LightGrenadier_CanRush :
        case CC_PTE_LightGrenadier_RushedPrevious :
        case CC_PTE_LightGrenadier_RushedCurrent : return "Grenadier";

        case CC_PTE_DarkScout_RushedCurrent :
        case CC_PTE_DarkScout_RushedPrevious :
        case CC_PTE_DarkScout_CanRush :
        case CC_PTE_DarkScout :
        case CC_PTE_LightScout :
        case CC_PTE_LightScout_CanRush :
        case CC_PTE_LightScout_RushedPrevious :
        case CC_PTE_LightScout_RushedCurrent : return "Scout";

        case CC_PTE_DarkCentaur :
        case CC_PTE_LightCentaur : return "Centaur";

        case CC_PTE_DarkWave :
        case CC_PTE_LightWave : return "Wave";

        case CC_PTE_DarkUnicorn :
        case CC_PTE_LightUnicorn : return "Unicorn";

        case CC_PTE_DarkPyramid :
        case CC_PTE_LightPyramid : return "Pyramid";

        case CC_PTE_DarkPegasus :
        case CC_PTE_LightPegasus : return "Pegasus";

        case CC_PTE_DarkKing_CanCastle :
        case CC_PTE_DarkKing :
        case CC_PTE_LightKing :
        case CC_PTE_LightKing_CanCastle : return "King";

        case CC_PTE_DarkQueen :
        case CC_PTE_LightQueen : return "Queen";

        case CC_PTE_DarkRook_CanCastle :
        case CC_PTE_DarkRook :
        case CC_PTE_LightRook :
        case CC_PTE_LightRook_CanCastle : return "Rook";

        case CC_PTE_DarkBishop :
        case CC_PTE_LightBishop : return "Bishop";

        case CC_PTE_DarkKnight :
        case CC_PTE_LightKnight : return "Knight";

        case CC_PTE_DarkPawn_DelayedPromotion :
        case CC_PTE_DarkPawn_RushedCurrent :
        case CC_PTE_DarkPawn_RushedPrevious :
        case CC_PTE_DarkPawn_CanRush :
        case CC_PTE_DarkPawn :
        case CC_PTE_LightPawn :
        case CC_PTE_LightPawn_CanRush :
        case CC_PTE_LightPawn_RushedPrevious :
        case CC_PTE_LightPawn_RushedCurrent :
        case CC_PTE_LightPawn_DelayedPromotion : return "Pawn";

        case CC_PTE_None :
            return empty_field ? ( capitalize ? "Empty field"
                                              : "empty field" )
                               : "";

        case CC_PTE_Monolith : return "Monolith";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}

char cc_piece_symbol( CcPieceTagType ptt ) {
    return toupper( cc_piece_as_char( ptt ) );
}

CcPieceTagType cc_piece_demoting_to( CcPieceTagType ptt ) {
    // No need to check validity here; IS_DARK(), IS_LIGHT() each check proper range.
    if ( CC_PIECE_IS_DARK( ptt ) ) return CC_PTE_DarkPawn;
    if ( CC_PIECE_IS_LIGHT( ptt ) ) return CC_PTE_LightPawn;
    return CC_PTE_None;
}

bool cc_piece_has_color( CcPieceTagType ptt ) {
    return CC_PIECE_IS_LIGHT( ptt ) || CC_PIECE_IS_DARK( ptt );
}

bool cc_piece_has_shade( CcPieceTagType ptt ) {
    return CC_PIECE_IS_STAR( ptt );
}

bool cc_piece_has_prefix( CcPieceTagType ptt ) {
    return cc_piece_has_color( ptt ) || cc_piece_has_shade( ptt );
}

char const * cc_piece_prefix( CcPieceTagType ptt, bool capitalize ) {
    // No need to check validity here; IS_DARK(), IS_LIGHT() each check proper range.
    if ( CC_PIECE_IS_DARK( ptt ) ) return capitalize ? "Dark" : "dark";
    else if ( CC_PIECE_IS_LIGHT( ptt ) ) return capitalize ? "Light" : "light";
    else if ( ptt == CC_PTE_DimStar ) return capitalize ? "Dim" : "dim";
    else if ( ptt == CC_PTE_BrightStar ) return capitalize ? "Bright" : "bright";
    else return "";
}

bool cc_piece_has_congruent_type( char symbol, CcPieceTagType ptt ) {
    char ps = cc_piece_symbol( ptt );
    return ( symbol == ps );
}

bool cc_piece_is_equal( char symbol, bool is_light, CcPieceTagType ptt ) {
    CcPieceTagType piece = cc_piece_from_symbol( symbol, is_light );
    return ( piece == ptt );
}

bool cc_piece_has_same_type( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ) {
    if ( ptt_1 == ptt_2 ) return true;
    if ( ptt_1 == cc_piece_opposite( ptt_2 ) ) return true;

    CcPieceTagType p_1 = cc_piece_strip_tag( ptt_1 );
    CcPieceTagType p_2 = cc_piece_strip_tag( ptt_2 );
    if ( p_1 == p_2 ) return true;

    return false;
}

bool cc_piece_has_same_color( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ) {
    // No need to check validity here; IS_DARK(), IS_LIGHT() each check proper range.
    if ( CC_PIECE_IS_LIGHT( ptt_1 ) && CC_PIECE_IS_LIGHT( ptt_2 ) )
        return true;

    if ( CC_PIECE_IS_DARK( ptt_1 ) && CC_PIECE_IS_DARK( ptt_2 ) )
        return true;

    return false;
}

bool cc_piece_has_same_shade( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ) {
    if ( ( ptt_1 == CC_PTE_BrightStar ) && ( ptt_2 == CC_PTE_BrightStar ) ) return true;
    if ( ( ptt_1 == CC_PTE_DimStar ) && ( ptt_2 == CC_PTE_DimStar ) ) return true;
    return false;
}

bool cc_piece_is_opposite( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ) {
    if ( !CC_PIECE_IS_VALID( ptt_1 ) ) return false;
    if ( !CC_PIECE_IS_VALID( ptt_2 ) ) return false;

    if ( ( !CC_PIECE_HAS_OWNER( ptt_1 ) ) || ( !CC_PIECE_HAS_OWNER( ptt_2 ) ) ) return false;

    return ( ptt_1 == cc_piece_opposite( ptt_2 ) );
}

bool cc_piece_has_same_owner( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ) {
    // No need to check validity here; IS_DARK(), IS_LIGHT() each check proper range.
    if ( CC_PIECE_IS_LIGHT( ptt_1 ) && CC_PIECE_IS_LIGHT( ptt_2 ) ) return true;
    if ( CC_PIECE_IS_DARK( ptt_1 ) && CC_PIECE_IS_DARK( ptt_2 ) ) return true;

    return false;
}

bool cc_piece_has_different_owner( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ) {
    // No need to check validity here; IS_DARK(), IS_LIGHT() each check proper range.
    if ( CC_PIECE_IS_LIGHT( ptt_1 ) && CC_PIECE_IS_DARK( ptt_2 ) ) return true;
    if ( CC_PIECE_IS_DARK( ptt_1 ) && CC_PIECE_IS_LIGHT( ptt_2 ) ) return true;

    return false;
}

bool cc_piece_is_owned_figure( CcPieceTagType ptt ) {
    // No need to check validity here; IS_DARK(), IS_LIGHT() each check proper range; IS_PAWN() checks selected pieces.
    if ( CC_PIECE_IS_PAWN( ptt ) ) return false;
    if ( CC_PIECE_IS_LIGHT( ptt ) ) return true;
    if ( CC_PIECE_IS_DARK( ptt ) ) return true;
    return false;
}

bool cc_piece_is_figure( CcPieceTagType ptt ) {
    if ( CC_PIECE_IS_STAR( ptt ) ) return true;
    if ( CC_PIECE_IS_MONOLITH( ptt ) ) return true;

    return cc_piece_is_owned_figure( ptt );
}

bool cc_piece_is_one_step( CcPieceTagType piece, CcPieceTagType activator ) {
    CcPieceTagType ptt = CC_PIECE_IS_WAVE( piece ) ? activator : piece;
    return CC_PIECE_IS_SINGLE_STEP( ptt ) || CC_PIECE_IS_ONE_STEP( ptt );
}

bool cc_piece_is_two_step( CcPieceTagType piece, CcPieceTagType activator ) {
    if ( CC_PIECE_IS_WAVE( piece ) ) {
        return CC_WAVE_IS_TWO_STEP( activator );
    } else {
        return CC_PIECE_IS_TWO_STEP( piece ) ||
               CC_PIECE_IS_SINGLE_STEP_ALTERNATING( piece );
    }
}

bool cc_piece_is_many_steps( CcPieceTagType piece ) {
    if ( CC_PIECE_IS_WAVE( piece ) ) return false;
    return CC_PIECE_HAS_NEW_STEP_AFTER_EACH( piece );
}

char const * cc_piece_as_string( CcPieceTagType ptt, bool capitalize, bool empty_field ) {
    switch ( ptt ) {
        case CC_PTE_DimStar : return capitalize ? "Dim Star" : "dim Star";
        case CC_PTE_BrightStar : return capitalize ? "Bright Star" : "bright Star";

        case CC_PTE_DarkStarchild : return capitalize ? "Dark Starchild" : "dark Starchild";
        case CC_PTE_LightStarchild : return capitalize ? "Light Starchild" : "light Starchild";

        case CC_PTE_DarkShaman : return capitalize ? "Dark Shaman" : "dark Shaman";
        case CC_PTE_LightShaman : return capitalize ? "Light Shaman" : "light Shaman";

        case CC_PTE_DarkSerpent : return capitalize ? "Dark Serpent" : "dark Serpent";
        case CC_PTE_LightSerpent : return capitalize ? "Light Serpent" : "light Serpent";

        case CC_PTE_DarkGrenadier_RushedCurrent : return capitalize ? "Dark Grenadier (rushed in current turn)" : "dark Grenadier (rushed in current turn)";
        case CC_PTE_DarkGrenadier_RushedPrevious : return capitalize ? "Dark Grenadier (rushed in previous turn)" : "dark Grenadier (rushed in previous turn)";
        case CC_PTE_DarkGrenadier_CanRush : return capitalize ? "Dark Grenadier (can rush)" : "dark Grenadier (can rush)";
        case CC_PTE_DarkGrenadier : return capitalize ? "Dark Grenadier" : "dark Grenadier";
        case CC_PTE_LightGrenadier : return capitalize ? "Light Grenadier" : "light Grenadier";
        case CC_PTE_LightGrenadier_CanRush : return capitalize ? "Light Grenadier (can rush)" : "light Grenadier (can rush)";
        case CC_PTE_LightGrenadier_RushedPrevious : return capitalize ? "Light Grenadier (rushed in previous turn)" : "light Grenadier (rushed in previous turn)";
        case CC_PTE_LightGrenadier_RushedCurrent : return capitalize ? "Light Grenadier (rushed in current turn)" : "light Grenadier (rushed in current turn)";

        case CC_PTE_DarkScout_RushedCurrent : return capitalize ? "Dark Scout (rushed in current turn)" : "dark Scout (rushed in current turn)";
        case CC_PTE_DarkScout_RushedPrevious : return capitalize ? "Dark Scout (rushed in previous turn)" : "dark Scout (rushed in previous turn)";
        case CC_PTE_DarkScout_CanRush : return capitalize ? "Dark Scout (can rush)" : "dark Scout (can rush)";
        case CC_PTE_DarkScout : return capitalize ? "Dark Scout" : "dark Scout";
        case CC_PTE_LightScout : return capitalize ? "Light Scout" : "light Scout";
        case CC_PTE_LightScout_CanRush : return capitalize ? "Light Scout (can rush)" : "light Scout (can rush)";
        case CC_PTE_LightScout_RushedPrevious : return capitalize ? "Light Scout (rushed in previous turn)" : "light Scout (rushed in previous turn)";
        case CC_PTE_LightScout_RushedCurrent : return capitalize ? "Light Scout (rushed in current turn)" : "light Scout (rushed in current turn)";

        case CC_PTE_DarkCentaur : return capitalize ? "Dark Centaur" : "dark Centaur";
        case CC_PTE_LightCentaur : return capitalize ? "Light Centaur" : "light Centaur";

        case CC_PTE_DarkWave : return capitalize ? "Dark Wave" : "dark Wave";
        case CC_PTE_LightWave : return capitalize ? "Light Wave" : "light Wave";

        case CC_PTE_DarkUnicorn : return capitalize ? "Dark Unicorn" : "dark Unicorn";
        case CC_PTE_LightUnicorn : return capitalize ? "Light Unicorn" : "light Unicorn";

        case CC_PTE_DarkPyramid : return capitalize ? "Dark Pyramid" : "dark Pyramid";
        case CC_PTE_LightPyramid : return capitalize ? "Light Pyramid" : "light Pyramid";

        case CC_PTE_DarkPegasus : return capitalize ? "Dark Pegasus" : "dark Pegasus";
        case CC_PTE_LightPegasus : return capitalize ? "Light Pegasus" : "light Pegasus";

        case CC_PTE_DarkKing_CanCastle : return capitalize ? "Dark King (can castle)" : "dark King (can castle)";
        case CC_PTE_DarkKing : return capitalize ? "Dark King" : "dark King";
        case CC_PTE_LightKing : return capitalize ? "Light King" : "light King";
        case CC_PTE_LightKing_CanCastle : return capitalize ? "Light King (can castle)" : "light King (can castle)";

        case CC_PTE_DarkQueen : return capitalize ? "Dark Queen" : "dark Queen";
        case CC_PTE_LightQueen : return capitalize ? "Light Queen" : "light Queen";

        case CC_PTE_DarkRook_CanCastle : return capitalize ? "Dark Rook (can castle)" : "dark Rook (can castle)";
        case CC_PTE_DarkRook : return capitalize ? "Dark Rook" : "dark Rook";
        case CC_PTE_LightRook : return capitalize ? "Light Rook" : "light Rook";
        case CC_PTE_LightRook_CanCastle : return capitalize ? "Light Rook (can castle)" : "light Rook (can castle)";

        case CC_PTE_DarkBishop : return capitalize ? "Dark Bishop" : "dark Bishop";
        case CC_PTE_LightBishop : return capitalize ? "Light Bishop" : "light Bishop";

        case CC_PTE_DarkKnight : return capitalize ? "Dark Knight" : "dark Knight";
        case CC_PTE_LightKnight : return capitalize ? "Light Knight" : "light Knight";

        case CC_PTE_DarkPawn_DelayedPromotion : return capitalize ? "Dark Pawn (delayed promotion)" : "dark Pawn (delayed promotion)";
        case CC_PTE_DarkPawn_RushedCurrent : return capitalize ? "Dark Pawn (rushed in current turn)" : "dark Pawn (rushed in current turn)";
        case CC_PTE_DarkPawn_RushedPrevious : return capitalize ? "Dark Pawn (rushed in previous turn)" : "dark Pawn (rushed in previous turn)";
        case CC_PTE_DarkPawn_CanRush : return capitalize ? "Dark Pawn (can rush)" : "dark Pawn (can rush)";
        case CC_PTE_DarkPawn : return capitalize ? "Dark Pawn" : "dark Pawn";
        case CC_PTE_LightPawn : return capitalize ? "Light Pawn" : "light Pawn";
        case CC_PTE_LightPawn_CanRush : return capitalize ? "Light Pawn (can rush)" : "light Pawn (can rush)";
        case CC_PTE_LightPawn_RushedPrevious : return capitalize ? "Light Pawn (rushed in previous turn)" : "light Pawn (rushed in previous turn)";
        case CC_PTE_LightPawn_RushedCurrent : return capitalize ? "Light Pawn (rushed in current turn)" : "light Pawn (rushed in current turn)";
        case CC_PTE_LightPawn_DelayedPromotion : return capitalize ? "Light Pawn (delayed promotion)" : "light Pawn (delayed promotion)";

        case CC_PTE_None :
            return empty_field ? ( capitalize ? "Empty field"
                                              : "empty field" )
                               : "";

        case CC_PTE_Monolith : return "Monolith";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}
