// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <ctype.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_piece.h"

#include "cc_parse_utils.h"

#include "cc_rule_utils.h"

/**
    @file cc_parse_utils.c
    @brief Helper functions to parse algebraic notation.
*/


bool cc_parse_utils_char_is_ply_gather( char const c )
{
    return ( ( c == '[' ) || ( c == ']' ) );
}

char const * cc_parse_utils_go_ply_gather( char const * const restrict move_str,
                                           bool const skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( ( *m != '\0' ) && cc_parse_utils_char_is_ply_gather( *m ) ) ++m;
    else
        while ( ( *m != '\0' ) && !cc_parse_utils_char_is_ply_gather( *m ) ) ++m;

    return m;
}

size_t cc_parse_utils_ply_link_len( char const * const restrict ply_str )
{
    if ( !ply_str ) return 0;

    char const * c = ply_str;

    if ( *c == '~' ) return 1;

    if ( *c == '|' )
    {
        if ( *++c == '|' ) return 2;
        return 1;
    }

    if ( *c == '@' )
    {
        if ( *++c == '@' )
        {
            if ( *++c == '@' ) return 3;
            return 2;
        }
        return 1;
    }

    if ( *c == ':' )
    {
        if ( *++c == ':' ) return 2;
        return 0; // En passant is not ply divider.
    }

    return 0;
}

char const * cc_parse_utils_go_ply_link( char const * const restrict move_str,
                                         bool const skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( *m != '\0' )
        {
            size_t len = cc_parse_utils_ply_link_len( m );

            if ( len > 0 )
                m += len;
            else
                break;
        }
    else
        while ( ( *m != '\0' ) && ( !cc_parse_utils_ply_link_len( m ) ) ) ++m;

    return m;
}

char * cc_parse_utils_next_ply_str_new( char const * const restrict move_str_s )
{
    /* static char const * move_start = NULL; */
    static char const * ply_start = NULL;
    static char const * ply_end = NULL;

    bool parse_1st = (bool)move_str_s;

    if ( move_str_s )
    {
        /* move_start = */ ply_start = ply_end = move_str_s;
    }

    if ( !ply_end ) return NULL;

    if ( *ply_end == '\0' )
    {
        ply_end = NULL; // Invalidate future calls without initialization.
        return NULL;
    }

    if ( !parse_1st )
        ply_start = cc_parse_utils_go_ply_link( ply_end, false );

    ply_end = cc_parse_utils_go_ply_link( ply_start, true );
    ply_end = cc_parse_utils_go_ply_link( ply_end, false );

    if ( ply_end == ply_start ) return NULL;

    size_t len = ply_end - ply_start;
    char * ply_str = malloc( len + 1 );
    if ( !ply_str ) return NULL;

    char const * in = ply_start;
    char * out = ply_str;

    while ( in < ply_end )
    {
        if ( !cc_parse_utils_char_is_ply_gather( *in ) )
            *out++ = *in++;
        else
            ++in;
    }

    *out = '\0';

    return ply_str;
}

bool cc_parse_utils_get_ply_link( char const * const restrict ply_str,
                                  CcPlyLinkEnum * const restrict link_o )
{
    if ( !ply_str ) return false;
    if ( !link_o ) return false;

    size_t len = cc_parse_utils_ply_link_len( ply_str );

    if ( len == 0 )
    {
        *link_o = CC_PLE_Ply;
        return true;
    }
    else if ( len == 1 )
    {
        char const c_0 = ply_str[ 0 ];

        if ( c_0 == '~' )
        {
            *link_o = CC_PLE_Ply;
            return true;
        }
        else if ( c_0 == '|' )
        {
            *link_o = CC_PLE_Teleportation;
            return true;
        }
        else if ( c_0 == '@' )
        {
            *link_o = CC_PLE_TranceJourney;
            return true;
        }
    }
    else if ( len == 2 )
    {
        char const c_0 = ply_str[ 0 ];
        char const c_1 = ply_str[ 1 ];

        if ( ( c_0 == '|' ) && ( c_1 == '|' ) )
        {
            *link_o = CC_PLE_FailedTeleportation;
            return true;
        }
        else if ( ( c_0 == '@' ) && ( c_1 == '@' ) )
        {
            *link_o = CC_PLE_DualTranceJourney;
            return true;
        }
        else if ( ( c_0 == ':' ) && ( c_1 == ':' ) )
        {
            *link_o = CC_PLE_PawnSacrifice;
            return true;
        }
    }
    else if ( len == 3 )
    {
        char const c_0 = ply_str[ 0 ];
        char const c_1 = ply_str[ 1 ];
        char const c_2 = ply_str[ 2 ];

        if ( ( c_0 == '@' ) && ( c_1 == '@' ) && ( c_2 == '@' ) )
        {
            *link_o = CC_PLE_FailedTranceJourney;
            return true;
        }
    }

    return false;
}

bool cc_parse_utils_get_ply_piece( char const * const restrict ply_str,
                                   bool const is_light,
                                   CcPieceEnum * const restrict piece_o )
{
    if ( !ply_str ) return false;
    if ( !piece_o ) return false;

    char const * p = ply_str;

    p = cc_parse_utils_go_ply_link( p, true );
    if ( !p ) return false;

    if ( isupper( *p ) ) // <!> Useage of cc_piece_is_symbol() here is bug,
                         //     all other upper chars would end as Pawns.
        *piece_o = cc_piece_from_symbol( *p, is_light );
    else
        *piece_o = ( is_light ) ? CC_PE_LightPawn : CC_PE_DarkPawn;

    return CC_PIECE_IS_VALID( *piece_o );
}


char const * cc_parse_utils_get_steps_str( char const * const restrict ply_str )
{
    if ( !ply_str ) return NULL;

    size_t len = cc_parse_utils_ply_link_len( ply_str );

    char const * p = ply_str + len;

    if ( isupper( *p ) ) ++p;

    return p;
}

size_t cc_parse_utils_step_link_len( char const * const restrict step_str )
{
    if ( !step_str ) return 0;

    char const * c = step_str;

    if ( *c == ',' ) return 1;

    if ( *c == '-' ) return 1;

    if ( *c == '.' )
    {
        if ( *++c == '.' ) return 2;
        return 1;
    }

    return 0;
}

char const * cc_parse_utils_go_step_link( char const * const restrict ply_str,
                                          bool const skip_or_stop_at )
{
    if ( !ply_str ) return NULL;

    char const * p = ply_str;

    if ( skip_or_stop_at )
        while ( *p != '\0' )
        {
            size_t len = cc_parse_utils_step_link_len( p );

            if ( len > 0 )
                p += len;
            else
                break;
        }
    else
        while ( ( *p != '\0' ) && ( !cc_parse_utils_step_link_len( p ) ) ) ++p;

    return p;
}

char * cc_parse_utils_next_step_str_new( char const * const restrict ply_str_s )
{
    /* static char const * ply_start = NULL; */
    static char const * step_start = NULL;
    static char const * step_end = NULL;

    bool parse_1st = (bool)ply_str_s;

    if ( ply_str_s )
    {
        /* ply_start = */ step_start = step_end = cc_parse_utils_get_steps_str( ply_str_s );
    }

    if ( !step_end ) return NULL;

    if ( *step_end == '\0' )
    {
        step_end = NULL; // Invalidate future calls without initialization.
        return NULL;
    }

    if ( !parse_1st )
        step_start = cc_parse_utils_go_step_link( step_end, false );

    step_end = cc_parse_utils_go_step_link( step_start, true );
    step_end = cc_parse_utils_go_step_link( step_end, false );

    if ( step_end == step_start ) return NULL;

    size_t len = step_end - step_start;
    char * step_str = malloc( len + 1 );
    if ( !step_str ) return NULL;

    char const * in = step_start;
    char * out = step_str;

    while ( in < step_end )
        *out++ = *in++;

    *out = '\0';

    return step_str;
}

bool cc_parse_utils_ply_has_multiple_steps( char const * const restrict ply_str )
{
    if ( !ply_str ) return false;

    char const * p = ply_str;

    do
    {
        if ( ( *p == '.' ) || ( *p == '-' ) || ( *p == ',' ) ) return true;
    }
    while ( *p++ );

    return false;
}

bool cc_parse_utils_get_step_link( char const * const restrict ply_str,
                                   char const * const restrict step_str,
                                   CcStepLinkEnum * const restrict link_o )
{
    if ( !ply_str ) return false;
    if ( !step_str ) return false;
    if ( !link_o ) return false;

    size_t len = cc_parse_utils_step_link_len( step_str );

    if ( len == 0 )
    {
        if ( cc_parse_utils_ply_has_multiple_steps( ply_str ) )
            *link_o = CC_SLE_Start;
        else
            *link_o = CC_SLE_Destination;

        return true;
    }
    else if ( len == 1 )
    {
        char const c_0 = step_str[ 0 ];

        if ( c_0 == '.' )
        {
            *link_o = CC_SLE_Next;
            return true;
        }
        else if ( c_0 == ',' )
        {
            *link_o = CC_SLE_Reposition;
            return true;
        }
        else if ( c_0 == '-' )
        {
            *link_o = CC_SLE_Destination;
            return true;
        }
    }
    else if ( len == 2 )
    {
        char const c_0 = step_str[ 0 ];
        char const c_1 = step_str[ 1 ];

        if ( ( c_0 == '.' ) && ( c_1 == '.' ) )
        {
            *link_o = CC_SLE_Distant;
            return true;
        }
    }

    return false;
}

char const * cc_parse_utils_stop_at_side_effects( char const * const restrict step_str )
{
    if ( !step_str ) return NULL;

    char const * p = step_str;

    while ( *p != '\0' )
    {
        switch ( *p )
        {
            case '*' :
            case '<' :
            case ':' :
            case '&' :
            case '=' :
            case '%' :
            case '>' :
            case '$' :
                return p;

            default :
                ++p;
        }
    }

    return p;
}

char * cc_parse_utils_step_fields_str_new( char const * const restrict step_str )
{
    if ( !step_str ) return NULL;

    char const * fields_start = cc_parse_utils_go_step_link( step_str, true );
    if ( !fields_start ) return NULL;

    char const * fields_end = cc_parse_utils_stop_at_side_effects( fields_start );
    if ( !fields_end ) return NULL;

    if ( fields_end == fields_start ) return NULL;

    size_t len = fields_end - fields_start;
    char * fields_str = malloc( len + 1 );
    if ( !fields_str ) return NULL;

    char const * in = fields_start;
    char * out = fields_str;

    while ( in < fields_end )
        *out++ = *in++;

    *out = '\0';

    return fields_str;
}

char const * cc_parse_utils_side_effect_str( char const * const restrict step_str )
{
    if ( !step_str ) return NULL;

    char const * side_effect = cc_parse_utils_stop_at_side_effects( step_str );
    // if ( !side_effect ) return NULL; // Not needed, doesn't do anything with side_effect.

    return side_effect;
}

bool cc_parse_utils_is_fields_str_valid( char const * const restrict fields_str )
{
    if ( !fields_str ) return false;

    // Max len of a fields string is 6, i.e.
    // 2 files + 2 ranks --> 2 chars + 2 * 2-digit ints --> 6 chars max.
    #define MAX_LEN (6)

    // Min len of a fields string is 2, i.e.
    // 1 file + 1 rank --> 1 char + 1 * 1-digit int --> 2 chars min.
    #define MIN_LEN (2)

    // By checking 1 char over maximum size,
    // we check if string is longer than expected.
    #define MAX_LEN_TO_CHECK (7)

    #define INVALID_POS (-1)

    size_t len = cc_str_len_min( fields_str, MAX_LEN_TO_CHECK );
    if ( len > MAX_LEN ) return false;
    if ( len < MIN_LEN ) return false;

    bool result = true;
    char const * f_str__o = cc_str_duplicate_len_new( fields_str, true, MAX_LEN_TO_CHECK );
    char const * f = f_str__o;

    if ( !isdigit( *f ) )
        result = false;
    else
    {
        ++f;

        if ( isdigit( *f ) )
            ++f;

        if ( !islower( *f ) )
            result = false;
        else
        {
            ++f;

            if ( isdigit( *f ) )
            {
                ++f;

                if ( isdigit( *f ) )
                    ++f;
            }

            if ( islower( *f ) )
                ++f;
        }
    }

    result = result && ( *f == '\0' );

    free( (char *)f_str__o );
    return result;
}

bool cc_parse_utils_get_fields( char const * const restrict fields_str,
                                CcChessboard const * const restrict cb,
                                int * restrict disambiguation_file_o,
                                int * restrict disambiguation_rank_o,
                                int * restrict file_o,
                                int * restrict rank_o )
{
    if ( !fields_str ) return false;
    if ( !disambiguation_file_o ) return false;
    if ( !disambiguation_rank_o ) return false;
    if ( !file_o ) return false;
    if ( !rank_o ) return false;

    if ( !cc_parse_utils_is_fields_str_valid( fields_str ) ) return false;

    int file_0 = *disambiguation_file_o = CC_INVALID_OFF_BOARD_COORD_MIN;
    int rank_0 = *disambiguation_rank_o = CC_INVALID_OFF_BOARD_COORD_MIN;
    int file_1 = *file_o = CC_INVALID_OFF_BOARD_COORD_MIN;
    int rank_1 = *rank_o = CC_INVALID_OFF_BOARD_COORD_MIN;

    char const * file_0_str = fields_str;
    if ( *file_0_str != '\0' )
    {
        bool is_file_0 = ( islower( *file_0_str ) ) ? true : false;
        file_0 = ( is_file_0 ) ? ( *file_0_str ) - 'a' : CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( !cc_chessboard_is_coord_on_board( cb, file_0 ) )
            file_0 = CC_INVALID_OFF_BOARD_COORD_MIN;

        char const * rank_0_str = file_0_str + is_file_0;
        if ( *rank_0_str != '\0' )
        {
            // <!> Needed, rank_0_len is used.
            //
            int rank_0_len = isdigit( *rank_0_str ) ? 1 : 0;
            if ( rank_0_len > 0 ) rank_0_len += isdigit( *( rank_0_str + 1 ) ) ? 1 : 0;
            rank_0 = ( rank_0_len > 0 ) ? atoi( rank_0_str ) - 1
                                        : CC_INVALID_OFF_BOARD_COORD_MIN;

            if ( !cc_chessboard_is_coord_on_board( cb, rank_0 ) )
                rank_0 = CC_INVALID_OFF_BOARD_COORD_MIN;

            char const * file_1_str = rank_0_str + rank_0_len;
            if ( *file_1_str != '\0' )
            {
                bool is_file_1 = ( islower( *file_1_str ) ) ? true : false;
                file_1 = ( is_file_1 ) ? ( *file_1_str ) - 'a' : CC_INVALID_OFF_BOARD_COORD_MIN;

                if ( !cc_chessboard_is_coord_on_board( cb, file_1 ) )
                    file_1 = CC_INVALID_OFF_BOARD_COORD_MIN;

                char const * rank_1_str = file_1_str + is_file_1;
                if ( *rank_1_str != '\0' )
                {
                    rank_1 = ( isdigit( *rank_1_str ) ) ? atoi( rank_1_str ) - 1
                                                        : CC_INVALID_OFF_BOARD_COORD_MIN;

                    if ( !cc_chessboard_is_coord_on_board( cb, rank_1 ) )
                        rank_1 = CC_INVALID_OFF_BOARD_COORD_MIN;
                }
            }
        }
    }

    if ( ( file_1 == CC_INVALID_OFF_BOARD_COORD_MIN ) && ( rank_1 == CC_INVALID_OFF_BOARD_COORD_MIN ) )
    {
        *file_o = file_0;
        *rank_o = rank_0;
    }
    else
    {
        *disambiguation_file_o = file_0;
        *disambiguation_rank_o = rank_0;
        *file_o = file_1;
        *rank_o = rank_1;
    }

    return true;
}

bool cc_parse_utils_get_side_effect( char const * const restrict step_str,
                                     CcChessboard const * const restrict cb,
                                     CcPieceEnum ply_piece,
                                     int const step_i,
                                     int const step_j,
                                     CcSideEffect * const restrict side_effect_o )
{
    if ( !step_str ) return false;
    if ( !cb ) return false;
    if ( !side_effect_o ) return false;

    char const * side_effect_str = cc_parse_utils_side_effect_str( step_str );
    if ( !side_effect_str ) return false;

    char const * s = side_effect_str;

    if ( *s == '*' )
    {
        CcPieceEnum piece = CC_PE_None;

        if ( isupper( *( s + 1 ) ) )
        {
            piece = cc_chessboard_get_piece( cb, step_i, step_j );
            if ( !CC_PIECE_IS_DISPOSABLE( piece ) )
                return false;

            CcPieceEnum pe = cc_piece_from_symbol( *++s, cc_piece_is_light( piece, true ) );
            if ( !CC_PIECE_IS_DISPOSABLE( pe ) )
                return false;

            if ( !cc_piece_is_the_same_type( piece, pe, true ) )
                return false;
        }

        CcTagEnum tag = cc_chessboard_get_tag( cb, step_i, step_j );
        bool is_promo_tag_lost = ( tag == CC_TE_DelayedPromotion );

        if ( ( *( s + 1 ) == '=' ) && ( *( s + 2 ) == '=' ) )
            if ( !is_promo_tag_lost )
                return false;

        *side_effect_o = cc_side_effect_capture( piece, is_promo_tag_lost );
        return true;
    }
    else if ( *s == '<' )
    {
        CcPieceEnum piece = CC_PE_None;

        if ( isupper( *( s + 1 ) ) )
        {
            piece = cc_chessboard_get_piece( cb, step_i, step_j );
            if ( !CC_PIECE_IS_DISPLACEABLE( piece ) )
                return false;

            CcPieceEnum pe = cc_piece_from_symbol( *++s, cc_piece_is_light( piece, true ) );
            if ( !CC_PIECE_IS_DISPLACEABLE( pe ) )
                return false;

            if ( !cc_piece_is_the_same_type( piece, pe, true ) )
                return false;
        }

        CcTagEnum tag = cc_chessboard_get_tag( cb, step_i, step_j );
        bool is_promo_tag_lost = ( tag == CC_TE_DelayedPromotion );

        if ( ( *( s + 1 ) == '=' ) && ( *( s + 2 ) == '=' ) )
        {
            if ( !is_promo_tag_lost )
                return false;
            else
                s += 2;
        }

        int dest_i = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( islower( *( s + 1 ) ) )
        {
            dest_i = ( *++s ) - 'a';

            if ( !cc_chessboard_is_coord_on_board( cb, dest_i ) )
                return false;
        }
        else
            return false;

        int dest_j = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( isdigit( *( s + 1 ) ) )
        {
            dest_j = atoi( ++s ) - 1;

            if ( !cc_chessboard_is_coord_on_board( cb, dest_j ) )
                return false;
        }
        else
            return false;

        *side_effect_o = cc_side_effect_displacement( piece, is_promo_tag_lost, dest_i, dest_j );
        return true;
    }
    else if ( *s == ':' )
    {
        if ( !CC_PIECE_IS_PAWN( ply_piece ) )
            return false;

        CcPieceEnum piece = CC_PE_None;
        int board_j = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( !cc_rule_utils_find_en_passant_target( cb, ply_piece, step_i, step_j, &piece, &board_j ) )
            return false;

        int dist_j = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( isdigit( *( s + 1 ) ) )
        {
            dist_j = atoi( ++s ) - 1;

            if ( dist_j != board_j )
                return false;
        }
        else
            return false;

        int dist_i = step_i;

        *side_effect_o = cc_side_effect_en_passant( piece, dist_i, dist_j );
        return true;
    }
    else if ( *s == '&' )
    {
        if ( !CC_PIECE_IS_KING( ply_piece ) )
            return false;

        CcPieceEnum rook = ( ply_piece == CC_PE_LightKing ) ? CC_PE_LightRook
                                                            : CC_PE_DarkRook;
// TODO :: FIX :: dest_i = off-board !!!
        int start_i = CC_INVALID_OFF_BOARD_COORD_MIN;
        int start_j = step_j;
// TODO :: FIX :: dest_i = off-board !!!
        int dest_i = CC_INVALID_OFF_BOARD_COORD_MIN;
        int dest_j = step_j;

        int dest_j_len = isdigit( *( s + 1 ) ) ? 1 : 0;
        // if ( dest_j_len > 0 ) dest_j_len += isdigit( *( s + 2 ) ) ? 1 : 0; // Not needed.
        if ( dest_j_len > 0 )
            dest_j = atoi( ++s ) - 1;
// TODO :: CHECK :: dest_j is on-board
        else
            return false;

        *side_effect_o = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
        return true;
    }
    else if ( *s == '=' )
    {
        CcPieceEnum piece = cc_chessboard_get_piece( cb, step_i, step_j );
        if ( !CC_PIECE_IS_PAWN( piece ) )
            return false;

        if ( isupper( *( s + 1 ) ) )
        {
            CcPieceEnum promote_to = cc_piece_from_symbol( *++s, cc_piece_is_light( piece, false ) );
            if ( !CC_PIECE_IS_PROMOTE_TO( promote_to ) )
                return false;

            *side_effect_o = cc_side_effect_promote( promote_to );
            return true;
        }
        else
        {
            *side_effect_o = cc_side_effect_tag_for_promotion();
            return true;
        }
    }
    else if ( *s == '%' )
    {
        if ( ( *( s + 1 ) ) == '%' )
        {
            *side_effect_o = cc_side_effect_failed_conversion();
            return true;
        }
        else if ( isupper( *( s + 1 ) ) )
        {
            if ( !CC_PIECE_IS_PYRAMID( ply_piece ) )
                return false;

            bool is_dark = !cc_piece_is_light( ply_piece, false );
            CcPieceEnum pe = cc_piece_from_symbol( *++s, is_dark );
            if ( !CC_PIECE_IS_VALID( pe ) )
                return false;

            CcPieceEnum piece = cc_chessboard_get_piece( cb, step_i, step_j );
            if ( piece != pe ) return false;

            CcTagEnum tag = cc_chessboard_get_tag( cb, step_i, step_j );
            bool is_promo_tag_lost = ( tag == CC_TE_DelayedPromotion );

            if ( ( *( s + 1 ) == '=' ) && ( *( s + 2 ) == '=' ) )
                if ( !is_promo_tag_lost )
                    return false;

            *side_effect_o = cc_side_effect_convert( piece, is_promo_tag_lost );
            return true;
        }
        else
            return false;
    }
    else if ( *s == '>' )
    {
        if ( CC_PIECE_IS_MONOLITH( ply_piece ) )
            return false;

        CcPieceEnum piece = CC_PE_None;
        int dest_i = CC_INVALID_OFF_BOARD_COORD_MIN;
        int dest_j = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( isupper( *( s + 1 ) ) )
        {
// TODO :: FIX !!!

// CcPieceEnum piece = cc_chessboard_get_piece( cb, step_i, step_j );
            // CcPieceEnum pe = cc_piece_from_symbol( *++s, cc_piece_is_light( piece, true ) );
            // if ( !CC_PIECE_IS_VALID( pe ) )
            //     return false;

            // if ( !cc_piece_is_the_same_type( piece, pe, true ) )
            //     return false;

// TODO :: FIX !!!
        }

        if ( islower( *( s + 1 ) ) )
        {
            dest_i = ( *++s ) - 'a';

            if ( !cc_chessboard_is_coord_on_board( cb, dest_i ) )
                return false;
        }
        else
            return false;

        if ( isdigit( *( s + 1 ) ) )
        {
            dest_j = atoi( ++s ) - 1;

            if ( !cc_chessboard_is_coord_on_board( cb, dest_j ) )
                return false;
        }
        else
            return false;

        *side_effect_o = cc_side_effect_demote( piece, dest_i, dest_j );
        return true;
    }
    else if ( *s == '$' )
    {
        if ( !CC_PIECE_IS_STARCHILD( ply_piece ) )
            return false;

        if ( ( *( s + 1 ) ) == '$' )
        {
            *side_effect_o = cc_side_effect_failed_resurrection();
            return true;
        }
        else if ( isupper( *( s + 1 ) ) )
        {

            CcPieceEnum piece = cc_piece_from_symbol( *++s, true );
            if ( !CC_PIECE_IS_VALID( piece ) )
                return false;

            int dest_i = CC_INVALID_OFF_BOARD_COORD_MIN;

            if ( islower( *( s + 1 ) ) )
            {
                dest_i = ( *++s ) - 'a';

                if ( !cc_chessboard_is_coord_on_board( cb, dest_i ) )
                    return false;
            }
            else
                return false;

            int dest_j = CC_INVALID_OFF_BOARD_COORD_MIN;

            if ( isdigit( *( s + 1 ) ) )
            {
                dest_j = atoi( ++s ) - 1;

                if ( !cc_chessboard_is_coord_on_board( cb, dest_j ) )
                    return false;
            }
            else
                return false;

            *side_effect_o = cc_side_effect_resurrect( piece, dest_i, dest_j );
            return true;
        }
    }
    if ( isupper( *s ) )
    {
        CcPieceEnum piece = cc_chessboard_get_piece( cb, step_i, step_j );
        if ( !CC_PIECE_IS_PAWN( piece ) )
            return false;

        CcPieceEnum promote_to = cc_piece_from_symbol( *s, cc_piece_is_light( piece, true ) );
        if ( !CC_PIECE_IS_PROMOTE_TO( promote_to ) )
            return false;

        *side_effect_o = cc_side_effect_promote( promote_to );
        return true;
    }
    else
    {
        *side_effect_o = cc_side_effect_none();
        return true;
    }

    return false;
}
