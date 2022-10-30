// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_pos.h"

/**
    @file cc_pos.c
    @brief Functions for positions, side-effects, linked list.
*/


//
// Positions.

CcPos cc_pos( int i, int j )
{
    CcPos pos = { .i = i, .j = j };
    return pos;
}

bool cc_pos_is_valid( CcPos pos )
{
    return ( CC_IS_COORD_2_VALID( pos.i, pos.j ) );
}

bool cc_pos_is_static_step( CcPos pos )
{
    return ( ( pos.i == 0 ) && ( pos.j == 0 ) );
}

bool cc_pos_is_disambiguation( CcPos pos )
{
    return ( CC_IS_COORD_VALID( pos.i ) || ( !CC_IS_COORD_VALID( pos.j ) ) );
}

bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 )
{
    return ( ( pos_1.i == pos_2.i ) && ( pos_1.j == pos_2.j ) );
}

bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 )
{
    bool is_file = ( CC_IS_COORD_VALID( pos_1.i ) &&
                     CC_IS_COORD_VALID( pos_2.i ) );

    if ( is_file && ( pos_1.i != pos_2.i ) )
        return false;

    bool is_rank = ( CC_IS_COORD_VALID( pos_1.j ) &&
                     CC_IS_COORD_VALID( pos_2.j ) );

    if ( is_rank && ( pos_1.j != pos_2.j ) )
        return false;

    return is_file || is_rank;
}

CcPos cc_pos_add( CcPos pos, CcPos step, unsigned int count )
{
    int i = CC_INVALID_COORD;
    int j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( pos.i ) && CC_IS_COORD_VALID( step.i ) )
        i = pos.i + count * step.i;

    if ( CC_IS_COORD_VALID( pos.j ) && CC_IS_COORD_VALID( step.j ) )
        j = pos.j + count * step.j;

    return cc_pos( i, j );
}

CcPos cc_pos_subtract( CcPos pos, CcPos step, unsigned int count )
{
    int i = CC_INVALID_COORD;
    int j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( pos.i ) && CC_IS_COORD_VALID( step.i ) )
        i = pos.i - count * step.i;

    if ( CC_IS_COORD_VALID( pos.j ) && CC_IS_COORD_VALID( step.j ) )
        j = pos.j - count * step.j;

    return cc_pos( i, j );
}

CcPos cc_pos_step( CcPos start, CcPos destination )
{
    int diff_i = destination.i - start.i;
    int diff_j = destination.j - start.j;

    int gcd = cc_gcd( diff_i, diff_j );
    if ( gcd == 0 ) return CC_POS_CAST_INVALID;

    diff_i /= gcd;
    diff_j /= gcd;

    return cc_pos( diff_i, diff_j );
}

int cc_pos_momentum( CcPos start, CcPos destination )
{
    int diff_i = destination.i - start.i;
    int diff_j = destination.j - start.j;

    int momentum = cc_gcd( diff_i, diff_j );

    return momentum;
}

bool cc_pos_to_short_string( CcPos pos,
                             cc_char_8 * restrict pos_str__o )
{
    if ( !pos_str__o ) return false;

    if ( CC_IS_COORD_2_ON_BOARD( CC_MAX_BOARD_SIZE, pos.i, pos.j ) )
    {
        snprintf( *pos_str__o,
                  CC_MAX_LEN_CHAR_8,
                  "%c%hhd",
                  CC_CONVERT_BYTE_INTO_FILE_CHAR( pos.i ),
                  (signed char)(pos.j + 1) );
    }
    else
    {
        int count = 0; // snprintf() doesn't count '\0'

        if ( ( -100 < pos.i ) && ( pos.i < 1000 ) )
            count = snprintf( *pos_str__o,
                              CC_MAX_LEN_CHAR_8,
                              "%hd,",
                              (signed short)pos.i );
        else
            count = snprintf( *pos_str__o, CC_MAX_LEN_CHAR_8, "*," );

        if ( count < 1 ) return false; // count can't be > 4

        char * p = ( (char *)pos_str__o + count );
        size_t size = CC_MAX_LEN_CHAR_8 - count;

        if ( ( -100 < pos.j ) && ( pos.j < 1000 ) )
            count = snprintf( p, size, "%hd", (signed short)pos.j );
        else
            count = snprintf( p, size, "*" );

        if ( count < 1 ) return false; // count can't be > 4
    }

    return true;
}

//
// Side-effects.

size_t cc_side_effect_an_len( CcSideEffectEnum see )
{
    switch ( see )
    {
        case CC_SEE_None : return 0; /**< Side-effect not found, uninitialized, or error happened. */
        case CC_SEE_Capturing : return 1; /* Capturing, corresponds to * (asterisk). */
        case CC_SEE_Displacement : return 1; /* Trance-journey displacement, correspondes to < (less-than). */
        case CC_SEE_EnPassant : return 1; /* En passant, corresponds to : (colon). */
        case CC_SEE_Castling : return 1; /* Castling, corresponds to & (ampersand). */
        case CC_SEE_Promotion : return 1; /* Promotion, corresponds to = (equal sign), optional. */
        case CC_SEE_PromotionNoSign : return 0; /* Promotion, without sign. */
        case CC_SEE_TagForPromotion : return 1; /* Tag for promotion, corresponds to = (equal sign). */
        case CC_SEE_Conversion : return 1; /* Conversion, corresponds to % (percent sign). */
        case CC_SEE_FailedConversion : return 2; /* Failed conversion, corresponds to %% (double percent sign). */
        case CC_SEE_DemotingToPawn : return 1; /* Syzygy, demoting to Pawn, corresponds to > (greater-than sign). */
        case CC_SEE_Resurrection : return 1; /* Syzygy, resurrection, corresponds to $ (dollar-sign). */
        case CC_SEE_FailedResurrection : return 2; /* Syzygy, failed resurrection, corresponds to $$ (dual dollar-sign). */

        default : return 0;
    }
}

char const * cc_side_effect_symbol( CcSideEffectEnum see )
{
    switch ( see )
    {
        case CC_SEE_None : return ""; /**< Side-effect not found, uninitialized, or error happened. */
        case CC_SEE_Capturing : return "*"; /* Capturing, corresponds to * (asterisk). */
        case CC_SEE_Displacement : return "<"; /* Trance-journey displacement, correspondes to < (less-than). */
        case CC_SEE_EnPassant : return ":"; /* En passant, corresponds to : (colon). */
        case CC_SEE_Castling : return "&"; /* Castling, corresponds to & (ampersand). */
        case CC_SEE_Promotion : return "="; /* Promotion, corresponds to = (equal sign), optional. */
        case CC_SEE_PromotionNoSign : return "="; /* Promotion, without sign. */
        case CC_SEE_TagForPromotion : return "="; /* Tag for promotion, corresponds to = (equal sign). */
        case CC_SEE_Conversion : return "%"; /* Conversion, corresponds to % (percent sign). */
        case CC_SEE_FailedConversion : return "%%"; /* Failed conversion, corresponds to %% (double percent sign). */
        case CC_SEE_DemotingToPawn : return ">"; /* Syzygy, demoting to Pawn, corresponds to > (greater-than sign). */
        case CC_SEE_Resurrection : return "$"; /* Syzygy, resurrection, corresponds to $ (dollar-sign). */
        case CC_SEE_FailedResurrection : return "$$"; /* Syzygy, failed resurrection, corresponds to $$ (dual dollar-sign). */

        default : return "?";
    }
}

CcSideEffect cc_side_effect( CcSideEffectEnum type, CcPieceEnum piece, CcPos pos )
{
    CcSideEffect se = { .type = type, .piece = piece, .pos = pos };
    return se;
}

bool cc_side_effect_is_equal( CcSideEffect se_1, CcSideEffect se_2 )
{
    return ( ( se_1.type == se_2.type ) &&
             ( se_1.piece == se_2.piece ) &&
             cc_pos_is_equal( se_1.pos, se_2.pos ) );
}

bool cc_side_effect_is_valid( CcSideEffect se )
{
    return ( !cc_side_effect_is_equal( se, CC_SIDE_EFFECT_CAST_INVALID ) );
}

bool cc_side_effect_to_short_str( CcSideEffect se,
                                  cc_char_16 * restrict se_str__o )
{
    if ( !cc_str_clear( *se_str__o, CC_MAX_LEN_CHAR_16 ) )
        return false;

    if ( se.type == CC_SEE_None )
        return true;

    char * se_end = (char *)(se_str__o);

    char const * see_str = cc_side_effect_symbol( se.type );
    se_end += cc_str_copy( see_str, NULL, 2, *se_str__o, CC_MAX_LEN_CHAR_16 );

    char piece = cc_piece_symbol( se.piece );
    *se_end++ = piece;

    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    if ( !cc_pos_to_short_string( se.pos, &pos_c8 ) )
        return false;

    size_t unused = CC_MAX_LEN_CHAR_16 - ( se_end - (char *)(se_str__o) );
    // Not used afterwards. /* se_end += */
    cc_str_copy( pos_c8, NULL, CC_MAX_LEN_CHAR_8, se_end, unused );

    return true;
}

//
// Linked positions.

CcPosLink * cc_pos_link__new( CcPos pos, CcSideEffect side_effect )
{
    CcPosLink * pl__t = malloc( sizeof( CcPosLink ) );
    if ( !pl__t ) return NULL;

    pl__t->pos = pos;
    pl__t->side_effect = side_effect;

    pl__t->next = NULL;

    return pl__t;
}

CcPosLink * cc_pos_link_append( CcPosLink * restrict pos_link__io,
                                CcPos pos,
                                CcSideEffect side_effect )
{
    if ( !pos_link__io ) return NULL;

    CcPosLink * pl__t = cc_pos_link__new( pos, side_effect );
    if ( !pl__t ) return NULL;

    CcPosLink * pl = pos_link__io;

    while ( pl->next ) pl = pl->next; // rewind

    pl->next = pl__t; // append // Ownership transfer --> pl__t is now weak pointer.

    return pl__t;
}

CcPosLink * cc_pos_link_append_if( CcPosLink ** restrict pos_link__io,
                                   CcPos pos,
                                   CcSideEffect side_effect )
{
    if ( !pos_link__io ) return NULL;

    CcPosLink * pl__w = NULL;

    if ( !*pos_link__io )
        *pos_link__io = pl__w = cc_pos_link__new( pos, side_effect );
    else
        pl__w = cc_pos_link_append( *pos_link__io, pos, side_effect );

    return pl__w;
}

bool cc_pos_link_free_all( CcPosLink ** restrict pos_link__f )
{
    if ( !pos_link__f ) return false;
    if ( !*pos_link__f ) return true;

    CcPosLink * pl = *pos_link__f;
    CcPosLink * tmp = NULL;

    while ( pl )
    {
        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *pos_link__f = NULL;
    return true;
}

size_t cc_pos_link_len( CcPosLink * restrict pos_link )
{
    if ( !pos_link ) return 0;

    size_t len = 0;
    CcPosLink * pl = pos_link;

    while ( pl )
    {
        ++len;
        pl = pl->next;
    }

    return len;
}

char * cc_pos_link_to_short_string__new( CcPosLink * restrict pos_link )
{
    if ( !pos_link ) return NULL;

    // unused len is certainly > 0, because pos_link != NULL
    signed int unused = cc_pos_link_len( pos_link ) *
                        ( CC_MAX_LEN_CHAR_8 + CC_MAX_LEN_CHAR_16 + 1 );
                        // CC_MAX_LEN_CHAR_8, for position
                        // +CC_MAX_LEN_CHAR_16, for side-effect
                        // +1, for separator '.' between positions

    char * pl_str__a = malloc( unused + 1 ); // +1, for '\0'
    if ( !pl_str__a ) return NULL;

    *pl_str__a = '\0';

    char * pl_str = pl_str__a;
    char * pl_end = pl_str;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    cc_char_16 se_c16 = CC_CHAR_16_EMPTY;
    CcPosLink * pl = pos_link;

    while ( pl && ( unused > 0 ) )
    {
        if ( pl != pos_link ) // Not 1st pos ...
        {
            *pl_str++ = '.';
            *pl_str = '\0';
        }

        if ( !cc_pos_to_short_string( pl->pos, &pos_c8 ) )
        {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl_end = cc_str_append_into( pl_str, unused, pos_c8, CC_MAX_LEN_CHAR_8 );
        if ( !pl_end )
        {
            CC_FREE( pl_str__a );
            return NULL;
        }

        unused -= ( pl_end - pl_str );
        pl_str = pl_end;

        if ( !cc_side_effect_to_short_str( pl->side_effect, &se_c16 ) )
        {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl_end = cc_str_append_into( pl_str, unused, se_c16, CC_MAX_LEN_CHAR_16 );
        if ( !pl_end )
        {
            CC_FREE( pl_str__a );
            return NULL;
        }

        unused -= ( pl_end - pl_str );
        pl_str = pl_end;

        pl = pl->next;
    }

    return pl_str__a;
}
