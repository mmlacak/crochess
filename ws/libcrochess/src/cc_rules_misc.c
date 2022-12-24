// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_str_utils.h"

#include "cc_path_defs.h"

#include "cc_rules_misc.h"
#include "cc_rules_path.h"


#define CC_DRAW_OFFER_LEN (3)

#define CC_DRAW_OFFER_CANCELED (-1)
#define CC_DRAW_OFFER_NOT_FOUND (0)
#define CC_DRAW_OFFER_FOUND (1)

static int cc_an_str_ends_with_draw_offer( char const * restrict an_start,
                                           char const * restrict an_end__d,
                                           size_t max_len__d )
{
    if ( !an_start ) return CC_DRAW_OFFER_NOT_FOUND;

    size_t len = cc_str_len( an_start, an_end__d, max_len__d );
    if ( len < CC_DRAW_OFFER_LEN ) return CC_DRAW_OFFER_NOT_FOUND;

    char const * c = an_start + len - 1;

    while ( !*c )
        if ( an_start < c )
            --c;
        else
            break;

    if ( c < an_start + CC_DRAW_OFFER_LEN - 1 )
        return CC_DRAW_OFFER_NOT_FOUND;

    // Do mind: reverse order!
    if ( *c == ')' )
    {
        if ( *--c == '=' )
        {
            if ( *--c == '(' )
            {
                // "(=)" draw offered
                return CC_DRAW_OFFER_FOUND;
            }
        }
        else if ( *c == '-' )
        {
            if ( *--c == '(' )
            {
                // "(-)" draw offer canceled
                return CC_DRAW_OFFER_CANCELED;
            }
        }
    }

    return CC_DRAW_OFFER_NOT_FOUND;
}


bool cc_check_valid_draw_offer_exists( CcMove * restrict moves,
                                       CcGameStatusEnum gse )
{
    if ( !moves ) return false;
    if ( !CC_GAME_STATUS_IS_TURN( gse ) ) return false;

    int draw_offer = CC_DRAW_OFFER_NOT_FOUND;
    CcMove * m = moves;
    while ( m->next ) m = m->next; // rewind to last

    while ( m )
    {
        draw_offer = cc_an_str_ends_with_draw_offer( m->notation,
                                                     NULL,
                                                     CC_MAX_LEN_ZERO_TERMINATED );

        if ( draw_offer == CC_DRAW_OFFER_CANCELED )
            return false;
        else if ( draw_offer == CC_DRAW_OFFER_FOUND )
            return true;

        m = m->prev;
        if ( m )
            m = m->prev;
        else
            break;
    }

    return false;
}

int cc_promoting_rank( CcChessboard * restrict cb, bool is_light )
{
    if ( !is_light ) return 0;
    if ( !cb ) return CC_INVALID_COORD;

    return cb->size - 1;
}

// TODO :: DELETE
//
// bool cc_check_tag_is_lost( CcTagEnum lost, CcTagEnum tag )
// {
//     switch ( lost )
//     {
//         case CC_TE_DelayedPromotion :
//         case CC_TE_CanRush :
//         case CC_TE_CanCastle :
//             return ( lost == tag );

//         default :
//             return false;
//     }
// }
//
// TODO :: DELETE

bool cc_check_promote_or_tag( CcChessboard * restrict cb,
                              CcPieceEnum pawn,
                              CcPos start,
                              CcPos destination )
{
    if ( !cb ) return false;
    if ( !CC_PIECE_IS_PAWN( pawn ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, start.i, start.j ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, destination.i, destination.j ) ) return false;

    if ( !cc_pos_is_equal( start, destination ) )
    {
        CcPos step = cc_pos_difference( destination, start );

        if ( cc_is_pawn_step( cb->type, pawn, step ) )
        {
            if ( !cc_is_pawn_step_valid( cb, pawn, start, destination ) )
                return false;
        }
        else if ( cc_is_pawn_capture_step( cb->type, pawn, step ) )
        {
            if ( !cc_is_pawn_capture_valid( cb, pawn, start, destination ) )
                return false;
        }
        else
            return false;

        // Movement (+ capture / activation) + promotion.

        bool is_light = cc_piece_is_light( pawn );
        int rank = cc_promoting_rank( cb, is_light );
        if ( !CC_IS_COORD_VALID( rank ) ) return false;

        if ( rank == destination.j ) return true;
    }
    else
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, destination.i, destination.j );
        if ( !CC_PIECE_IS_THE_SAME( pe, pawn ) ) return false;

        // Static promotion.

        CcTagEnum te = cc_chessboard_get_tag( cb, destination.i, destination.j );
        if ( CC_TAG_CAN_PROMOTE( te ) ) return true;
    }

    return false;
}

bool cc_delete_en_passant_tag( CcChessboard * restrict cb )
{
    if ( !cb ) return false;

    unsigned int count = 0;

    for ( int i = 0; i < (int)cb->size; ++i )
    {
        for ( int j = 0; j < (int)cb->size; ++j )
        {
            CcTagEnum te = cc_chessboard_get_tag( cb, i, j );

            if ( te == CC_TE_EnPassant )
            {
                if ( !cc_chessboard_set_tag( cb, i, j, CC_TE_None ) )
                    return false;

                ++count;
            }
        }
    }

    return ( count <= 1 );
}
