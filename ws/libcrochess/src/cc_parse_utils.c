// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_piece.h"

#include "cc_parse_utils.h"

#include "cc_rules_misc.h"

/**
    @file cc_parse_utils.c
    @brief Helper functions to parse algebraic notation.
*/


bool cc_parse_utils_char_is_ply_gather( char c )
{
    return ( ( c == '[' ) || ( c == ']' ) );
}

size_t cc_parse_utils_ply_link_len( char const * restrict ply_str )
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
        if ( *++c == ':' )
            if ( *++c == ':' ) return 3;
        return 0; // En passant, losing rush are not ply dividers.
    }

    return 0;
}

char const * cc_parse_utils_go_ply_link( char const * restrict move_str,
                                         bool skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * str__w = move_str;

    if ( skip_or_stop_at )
        while ( *str__w != '\0' )
        {
            size_t len = cc_parse_utils_ply_link_len( str__w );

            if ( len > 0 )
                str__w += len;
            else
                break;
        }
    else
        while ( ( *str__w != '\0' ) &&
                ( cc_parse_utils_ply_link_len( str__w ) == 0 ) )
            ++str__w;

    return str__w;
}

// TODO :: CONVERT :: new iterator template
bool cc_parse_utils_ply_str_iter__new( char const * restrict move_str,
                                       char ** restrict ply_an__o,
                                       bool initialize_iter )
{
    if ( !move_str ) return false;
    if ( !ply_an__o ) return false;
    if ( *ply_an__o ) return false;

    static char const * ply_start = NULL;
    static char const * ply_end = NULL;

    if ( initialize_iter )
    {
        ply_start = ply_end = move_str;
    }

    if ( !ply_end ) return false;

    if ( *ply_end == '\0' )
    {
        ply_end = NULL; // Invalidate future calls without initialization.
        return false;
    }

    if ( !initialize_iter ) // If not first call in a sequence.
        ply_start = cc_parse_utils_go_ply_link( ply_end, false );

    ply_end = cc_parse_utils_go_ply_link( ply_start, true );
    ply_end = cc_parse_utils_go_ply_link( ply_end, false );

    if ( ply_end == ply_start ) return false;

    size_t len = ply_end - ply_start;
    char * ply_str__t = malloc( len + 1 );
    if ( !ply_str__t ) return false;

    char const * in = ply_start;
    char * out = ply_str__t;

    while ( in < ply_end )
    {
        if ( !cc_parse_utils_char_is_ply_gather( *in ) )
            *out++ = *in++;
        else
            ++in;
    }

    *out = '\0';

    *ply_an__o = ply_str__t; // Ownership transfer --> ply_str__t is now weak pointer.

    return true;
}
// TODO :: CONVERT :: new iterator template

bool cc_parse_utils_ply_str_iter( char const * restrict move_str,
                                  char const ** restrict ply_first__io,
                                  char const ** restrict ply_end__io )
{
    if ( !move_str ) return false;
    if ( !ply_first__io ) return false;
    if ( !ply_end__io ) return false;

    if ( !( *ply_first__io ) && !( *ply_end__io ) )
        *ply_first__io = move_str;
    else if ( ( *ply_first__io ) && ( *ply_end__io ) )
        *ply_first__io = cc_parse_utils_go_ply_link( *ply_end__io, false );
    else
        return false;

    *ply_end__io = cc_parse_utils_go_ply_link( *ply_first__io, true );
    *ply_end__io = cc_parse_utils_go_ply_link( *ply_end__io, false );

    if ( ( **ply_first__io == '\0' ) || ( *ply_end__io == *ply_first__io ) )
    {
        *ply_first__io = *ply_end__io = NULL;
        return false;
    }

    return true;
}

bool cc_parse_utils_get_ply_link( char const * restrict ply_str,
                                  CcPlyLinkEnum * restrict link__o )
{
    if ( !ply_str ) return false;
    if ( !link__o ) return false;

    size_t len = cc_parse_utils_ply_link_len( ply_str );

    if ( len == 0 )
    {
        *link__o = CC_PLE_Ply;
        return true;
    }
    else if ( len == 1 )
    {
        char c_0 = ply_str[ 0 ];

        if ( c_0 == '~' )
        {
            *link__o = CC_PLE_Ply;
            return true;
        }
        else if ( c_0 == '|' )
        {
            *link__o = CC_PLE_Teleportation;
            return true;
        }
        else if ( c_0 == '@' )
        {
            *link__o = CC_PLE_TranceJourney;
            return true;
        }
    }
    else if ( len == 2 )
    {
        char c_0 = ply_str[ 0 ];
        char c_1 = ply_str[ 1 ];

        if ( ( c_0 == '|' ) && ( c_1 == '|' ) )
        {
            *link__o = CC_PLE_FailedTeleportation;
            return true;
        }
        else if ( ( c_0 == '@' ) && ( c_1 == '@' ) )
        {
            *link__o = CC_PLE_DualTranceJourney;
            return true;
        }
    }
    else if ( len == 3 )
    {
        char c_0 = ply_str[ 0 ];
        char c_1 = ply_str[ 1 ];
        char c_2 = ply_str[ 2 ];

        if ( ( c_0 == '@' ) && ( c_1 == '@' ) && ( c_2 == '@' ) )
        {
            *link__o = CC_PLE_FailedTranceJourney;
            return true;
        }
        else if ( ( c_0 == ':' ) && ( c_1 == ':' ) && ( c_2 == ':' ) )
        {
            *link__o = CC_PLE_PawnSacrifice;
            return true;
        }
    }

    return false;
}

bool cc_parse_utils_get_ply_piece( char const * restrict ply_str,
                                   bool is_light,
                                   CcPieceEnum * restrict piece__o )
{
    if ( !ply_str ) return false;
    if ( !piece__o ) return false;

    char const * p = ply_str;

    p = cc_parse_utils_go_ply_link( p, true );
    if ( !p ) return false;

    if ( isupper( *p ) ) // <!> Useage of cc_piece_is_symbol() here is bug,
                         //     all other upper chars would end as Pawns.
        *piece__o = cc_piece_from_symbol( *p, is_light );
    else
        *piece__o = ( is_light ) ? CC_PE_LightPawn : CC_PE_DarkPawn;

    return CC_PIECE_IS_VALID( *piece__o );
}

bool cc_parse_utils_get_ply_piece_symbol( char const * restrict ply_str,
                                          char * restrict piece_symbol__o )
{
    if ( !ply_str ) return false;
    if ( !piece_symbol__o ) return false;

    char const * p = ply_str;

    p = cc_parse_utils_go_ply_link( p, true );
    if ( !p ) return false;

    if ( isupper( *p ) ) // <!> Useage of cc_piece_is_symbol() here is bug,
                         //     all other upper chars would end as Pawns.
        *piece_symbol__o = *p;
    else
        *piece_symbol__o = 'P';

    return cc_piece_is_symbol( *piece_symbol__o );
}


char const * cc_parse_utils_get_steps_str( char const * restrict ply_str )
{
    if ( !ply_str ) return NULL;

    size_t len = cc_parse_utils_ply_link_len( ply_str );

    char const * steps__w = ply_str + len;

    if ( isupper( *steps__w ) ) ++steps__w;

    return steps__w;
}

size_t cc_parse_utils_step_link_len( char const * restrict step_str )
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

char const * cc_parse_utils_go_step_link( char const * restrict ply_str,
                                          bool skip_or_stop_at )
{
    if ( !ply_str ) return NULL;

    char const * str__w = ply_str;

    if ( skip_or_stop_at )
        while ( *str__w != '\0' )
        {
            size_t len = cc_parse_utils_step_link_len( str__w );

            if ( len > 0 )
                str__w += len;
            else
                break;
        }
    else
        while ( ( *str__w != '\0' ) && ( !cc_parse_utils_step_link_len( str__w ) ) ) ++str__w;

    return str__w;
}

char * cc_parse_utils_next_step_str__new( char const * restrict ply_str__s )
{
// TODO :: REMOVE :: static variables
    /* static char * ply_start = NULL; */
    static char const * step_start = NULL;
    static char const * step_end = NULL;
// TODO :: REMOVE :: static variables

    bool is_first = (bool)ply_str__s;

    if ( ply_str__s )
    {
        /* ply_start = */ step_start = step_end = cc_parse_utils_get_steps_str( ply_str__s );
    }

    if ( !step_end ) return NULL;

    if ( *step_end == '\0' )
    {
        step_end = NULL; // Invalidate future calls without initialization.
        return NULL;
    }

    if ( !is_first )
        step_start = cc_parse_utils_go_step_link( step_end, false );

    step_end = cc_parse_utils_go_step_link( step_start, true );
    step_end = cc_parse_utils_go_step_link( step_end, false );

    if ( step_end == step_start ) return NULL;

    size_t len = step_end - step_start;
    char * step_str__a = malloc( len + 1 );
    if ( !step_str__a ) return NULL;

    char const * in = step_start;
    char * out = step_str__a;

    while ( in < step_end )
        *out++ = *in++;

    *out = '\0';

    return step_str__a;
}

bool cc_parse_utils_ply_has_multiple_steps( char const * restrict ply_str )
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

bool cc_parse_utils_get_step_link( char const * restrict ply_str,
                                   char const * restrict step_str,
                                   CcStepLinkEnum * restrict link__o )
{
    if ( !ply_str ) return false;
    if ( !step_str ) return false;
    if ( !link__o ) return false;

    size_t len = cc_parse_utils_step_link_len( step_str );

    if ( len == 0 )
    {
        if ( cc_parse_utils_ply_has_multiple_steps( ply_str ) )
            *link__o = CC_SLE_Start;
        else
            *link__o = CC_SLE_Destination;

        return true;
    }
    else if ( len == 1 )
    {
        char c_0 = step_str[ 0 ];

        if ( c_0 == '.' )
        {
            *link__o = CC_SLE_Next;
            return true;
        }
        else if ( c_0 == ',' )
        {
            *link__o = CC_SLE_Reposition;
            return true;
        }
        else if ( c_0 == '-' )
        {
            *link__o = CC_SLE_Destination;
            return true;
        }
    }
    else if ( len == 2 )
    {
        char c_0 = step_str[ 0 ];
        char c_1 = step_str[ 1 ];

        if ( ( c_0 == '.' ) && ( c_1 == '.' ) )
        {
            *link__o = CC_SLE_Distant;
            return true;
        }
    }

    return false;
}

char const * cc_parse_utils_stop_at_side_effects( char const * restrict step_str )
{
    if ( !step_str ) return NULL;

    char const * str__w = step_str;

    while ( *str__w != '\0' )
    {
        switch ( *str__w )
        {
            case '*' :
            case '<' :
            case ':' :
            case '&' :
            case '=' :
            case '%' :
            case '>' :
            case '$' :
                return str__w;

            default :
                ++str__w;
        }
    }

    return str__w;
}

char * cc_parse_utils_step_fields_str__new( char const * restrict step_str )
{
    if ( !step_str ) return NULL;

    char const * fields_start = cc_parse_utils_go_step_link( step_str, true );
    if ( !fields_start ) return NULL;

    char const * fields_end = cc_parse_utils_stop_at_side_effects( fields_start );
    if ( !fields_end ) return NULL;

    if ( fields_end == fields_start ) return NULL;

    size_t len = fields_end - fields_start;
    char * fields_str__a = malloc( len + 1 );
    if ( !fields_str__a ) return NULL;

    char const * in = fields_start;
    char * out = fields_str__a;

    while ( in < fields_end )
        *out++ = *in++;

    *out = '\0';

    return fields_str__a;
}

char const * cc_parse_utils_side_effect_str( char const * restrict step_str )
{
    if ( !step_str ) return NULL;

    char const * str_se__w = cc_parse_utils_stop_at_side_effects( step_str );
    // if ( !str_se__w ) return NULL; // Not needed, doesn't do anything with side_effect.

    return str_se__w;
}

bool cc_parse_utils_is_fields_str_valid( char const * restrict fields_str )
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

    size_t len = cc_str_len( fields_str, NULL, MAX_LEN_TO_CHECK );
    if ( len > MAX_LEN ) return false;
    if ( len < MIN_LEN ) return false;

    bool result = true;
    char * f_str__a = cc_str_duplicate__new( fields_str, true, MAX_LEN_TO_CHECK );
    char * f = f_str__a;

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

    CC_FREE( f_str__a );

    return result;
}

bool cc_parse_utils_get_fields( char const * restrict fields_str,
                                CcChessboard * restrict cb,
                                int * restrict disambiguation_file__o,
                                int * restrict disambiguation_rank__o,
                                int * restrict file__o,
                                int * restrict rank__o )
{
    if ( !fields_str ) return false;
    if ( !disambiguation_file__o ) return false;
    if ( !disambiguation_rank__o ) return false;
    if ( !file__o ) return false;
    if ( !rank__o ) return false;

    if ( !cc_parse_utils_is_fields_str_valid( fields_str ) ) return false;

    int file_0 = *disambiguation_file__o = CC_INVALID_OFF_BOARD_COORD_MIN;
    int rank_0 = *disambiguation_rank__o = CC_INVALID_OFF_BOARD_COORD_MIN;
    int file_1 = *file__o = CC_INVALID_OFF_BOARD_COORD_MIN;
    int rank_1 = *rank__o = CC_INVALID_OFF_BOARD_COORD_MIN;

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
        *file__o = file_0;
        *rank__o = rank_0;
    }
    else
    {
        *disambiguation_file__o = file_0;
        *disambiguation_rank__o = rank_0;
        *file__o = file_1;
        *rank__o = rank_1;
    }

    return true;
}

bool cc_parse_utils_get_lost_tag( char const * restrict lost_tag_str,
                                  CcChessboard * restrict cb,
                                  int step_i,
                                  int step_j,
                                  CcTagEnum * restrict lost_tag__o )
{
    if ( !lost_tag_str ) return false;
    if ( !cb ) return false;
    if ( !lost_tag__o ) return false;

    char const * s = lost_tag_str;

    CcTagEnum tag = cc_chessboard_get_tag( cb, step_i, step_j );
    CcTagEnum lost_tag = CC_TE_None;

    if ( ( *( s + 1 ) == '=' ) && ( *( s + 2 ) == '=' ) )
        lost_tag = CC_TE_DelayedPromotion;
    else if ( ( *( s + 1 ) == '&' ) && ( *( s + 2 ) == '&' ) )
        lost_tag = CC_TE_CanCastle;
    else if ( ( *( s + 1 ) == ':' ) && ( *( s + 2 ) == ':' ) )
        lost_tag = CC_TE_CanRush;

    if ( CC_TAG_IS_LASTING( tag ) && ( tag != lost_tag ) )
        return false;

    *lost_tag__o = lost_tag;

    return true;
}

bool cc_parse_utils_get_side_effect( char const * restrict step_str,
                                     CcChessboard * restrict cb,
                                     CcPieceEnum ply_piece,
                                     int step_i,
                                     int step_j,
                                     CcSideEffect * restrict side_effect__o )
{
    if ( !step_str ) return false;
    if ( !cb ) return false;
    if ( !side_effect__o ) return false;

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

            if ( !CC_PIECE_IS_THE_SAME( piece, pe ) )
                return false;
        }

        CcTagEnum lost_tag = CC_TE_None;
        if ( !cc_parse_utils_get_lost_tag( s, cb, step_i, step_j, &lost_tag ) )
            return false;

        *side_effect__o = cc_side_effect_capture( piece, lost_tag );
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

            if ( !CC_PIECE_IS_THE_SAME( piece, pe ) )
                return false;
        }

        CcTagEnum lost_tag = CC_TE_None;
        if ( !cc_parse_utils_get_lost_tag( s, cb, step_i, step_j, &lost_tag ) )
            return false;
        else
            s += CC_PARSE_STR_LOST_TAG_LENGTH;

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

        *side_effect__o = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
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

        *side_effect__o = cc_side_effect_en_passant( piece, dist_i, dist_j );
        return true;
    }
    else if ( *s == '&' )
    {
        if ( !CC_PIECE_IS_KING( ply_piece ) )
            return false;

        CcPieceEnum rook = ( ply_piece == CC_PE_LightKing ) ? CC_PE_LightRook
                                                            : CC_PE_DarkRook;
        int start_i = CC_INVALID_OFF_BOARD_COORD_MIN;
        int start_j = step_j;
        int dest_i = CC_INVALID_OFF_BOARD_COORD_MIN;
        int dest_j = step_j;

        int dest_i_len = isdigit( *( s + 1 ) ) ? 1 : 0;
        // if ( dest_i_len > 0 ) dest_i_len += isdigit( *( s + 2 ) ) ? 1 : 0; // Not needed.
        if ( dest_i_len > 0 )
            dest_i = atoi( ++s ) - 1;

        if ( !cc_rule_utils_find_castling_rook( cb, ply_piece, step_i, step_j, &dest_i, &rook, &start_i ) )
            return false;

        *side_effect__o = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
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

            *side_effect__o = cc_side_effect_promote( promote_to );
            return true;
        }
        else
        {
            *side_effect__o = cc_side_effect_tag_for_promotion();
            return true;
        }
    }
    else if ( *s == '%' )
    {
        if ( ( *( s + 1 ) ) == '%' )
        {
            *side_effect__o = cc_side_effect_failed_conversion();
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

            CcTagEnum lost_tag = CC_TE_None;
            if ( !cc_parse_utils_get_lost_tag( s, cb, step_i, step_j, &lost_tag ) )
                return false;

            *side_effect__o = cc_side_effect_convert( piece, lost_tag );
            return true;
        }
        else
            return false;
    }
    else if ( *s == '>' )
    {
        if ( !CC_PIECE_IS_MONOLITH( ply_piece ) )
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

            // if ( !CC_PIECE_IS_THE_SAME( piece, pe ) )
            //     return false;

// TODO :: FIX !!!
        }

        CcTagEnum lost_tag = CC_TE_None;
        if ( !cc_parse_utils_get_lost_tag( s, cb, step_i, step_j, &lost_tag ) )
            return false;
        else
            s += CC_PARSE_STR_LOST_TAG_LENGTH;

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

        *side_effect__o = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
        return true;
    }
    else if ( *s == '$' )
    {
        if ( !CC_PIECE_IS_STARCHILD( ply_piece ) )
            return false;

        if ( ( *( s + 1 ) ) == '$' )
        {
            *side_effect__o = cc_side_effect_failed_resurrection();
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

            *side_effect__o = cc_side_effect_resurrect( piece, dest_i, dest_j );
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

        *side_effect__o = cc_side_effect_promote( promote_to );
        return true;
    }
    else
    {
        *side_effect__o = cc_side_effect_none();
        return true;
    }

    return false;
}
