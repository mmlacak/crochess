// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdio.h> // TEMP

#include "cc_str_utils.h" // TEMP

#include "cc_tag.h"

#include "cc_pos_defs.h"
#include "cc_pos_utils.h"

#include "cc_checks.h"
#include "cc_parse_utils.h"
#include "cc_parse_step.h"
#include "cc_parse_ply.h"


static bool _cc_fail_with_msg_invalid_ply_link( char const * ply_start_an,
                                                char const * ply_end_an,
                                                CcParseMsg ** parse_msgs__iod ) {
    char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid ply separator in ply '%s'.\n", ply_str__a );
    CC_FREE( ply_str__a );
    return false;
}

static bool _cc_fail_with_msg_invalid_first_ply_link( CcPlyLinkTypeEnum plte,
                                                      char const * ply_start_an,
                                                      char const * ply_end_an,
                                                      CcParseMsg ** parse_msgs__iod ) {
    char const * plte_str = cc_ply_link_type_symbol( plte );
    char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Found ply separator '%s', expected none in the first ply '%s'.\n", plte_str, ply_str__a );
    CC_FREE( ply_str__a );
    return false;
}

static bool _cc_fail_with_msg_invalid_piece_symbol( char piece_symbol,
                                                    char const * ply_start_an,
                                                    char const * ply_end_an,
                                                    CcParseMsg ** parse_msgs__iod ) {
    char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Found invalid piece symbol '%c'; in ply '%s'.\n", piece_symbol, ply_an__a );
    CC_FREE( ply_an__a );
    return false;
}

static bool _cc_fail_with_msg_piece_cannot_lose_tag( CcPieceType piece,
                                                     CcLosingTagType ltt,
                                                     char const * ply_start_an,
                                                     char const * ply_end_an,
                                                     CcParseMsg ** parse_msgs__iod ) {
    char const * piece_str = cc_piece_as_string( piece, true, true );
    char const * ltt_str = cc_losing_tag_as_string( ltt, false, true );
    char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot lose %s tag; in ply '%s'.\n", piece_str, ltt_str, ply_an__a );
    CC_FREE( ply_an__a );
    return false;
}

static bool _cc_parse_ply( char const * ply_start_an,
                           char const * ply_end_an,
                           bool is_turn_light,
                           cc_uint_t board_size,
                           bool is_first_ply,
                           CcPly ** ply__o,
                           CcParseMsg ** parse_msgs__iod ) {
    if ( !ply__o || *ply__o ) return false;

    //
    // Ply link.

    CcPlyLinkTypeEnum plte_an = cc_parse_ply_link( ply_start_an );
    if ( plte_an == CC_PLTE_None ) // Shouldn't fail, unless there's bug in cc_iter_ply().
        return _cc_fail_with_msg_invalid_ply_link( ply_start_an, ply_end_an, parse_msgs__iod );

    if ( is_first_ply && ( plte_an != CC_PLTE_StartingPly ) )
        return _cc_fail_with_msg_invalid_first_ply_link( plte_an, ply_start_an, ply_end_an, parse_msgs__iod );

    char const * c_an = ply_start_an + cc_ply_link_len( plte_an );

    if ( *c_an == '[' ) ++c_an; // Move past ply gathering.

    //
    // Piece symbol.

    char piece_symbol = ' ';

    if ( !CC_MAYBE_IS_TRUE( cc_fetch_piece_symbol( c_an, &piece_symbol, true ) ) )
        return _cc_fail_with_msg_invalid_piece_symbol( *c_an, ply_start_an, ply_end_an, parse_msgs__iod );

    CcPieceType pt_an = cc_piece_from_symbol( piece_symbol, is_turn_light ); // Piece type should be correct, but color (owner) might not be, if activating opponent's pieces (if not first ply).

    if ( cc_piece_symbol_is_valid( *c_an ) ) ++c_an;

    //
    // Losing tag.

    CcLosingTagType ltt_an = cc_parse_losing_tag( c_an );

    if ( !cc_check_piece_can_lose_tag( pt_an, ltt_an ) )
        return _cc_fail_with_msg_piece_cannot_lose_tag( pt_an, ltt_an, ply_start_an, ply_end_an, parse_msgs__iod );

    c_an += cc_losing_tag_len( ltt_an );

    //
    // Steps.

    CcStep * steps__t = NULL;

    if ( !cc_parse_steps( c_an, ply_end_an, is_turn_light, board_size, &steps__t, parse_msgs__iod ) ) {
        cc_step_free_all( &steps__t );
        return false;
    }
    // steps__t = NULL; // Not needed, ownership transferred.

    //
    // Create a new ply, which takes ownership of steps (steps__t).

    *ply__o = cc_ply__new( plte_an, pt_an, ltt_an, &steps__t );
    if ( !*ply__o ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    return true;
}


bool cc_parse_plies( char const * move_an,
                     bool is_turn_light,
                     cc_uint_t board_size,
                     CcPly ** plies__o,
                     CcParseMsg ** parse_msgs__iod ) {
    if ( !move_an ) return false;
    if ( !plies__o || *plies__o ) return false;
    if ( !parse_msgs__iod ) return false;

    CcPly * plies__t = NULL;
    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;
    CcPosDesc before_ply = CC_POS_DESC_CAST_INVALID;
    bool is_first_ply = true;

    while ( cc_iter_ply( move_an, &ply_start_an, &ply_end_an ) ) {
        CcPly * ply__t = NULL;

        if ( !_cc_parse_ply( ply_start_an, ply_end_an, is_turn_light, board_size, is_first_ply,
                             &ply__t,
                             parse_msgs__iod ) ) {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );

            printf( "!_cc_parse_ply( ... )\n" ); // TODO :: DEBUG :: DELETE

            return false;
        }


        { // TODO :: DEBUG :: DELETE
            char * plies_str__a = cc_ply_all_to_string__new( ply__t );

            // cc_str_print( plies_str__a, NULL, 0, "Ply: '%s'.\n", 0, NULL );
            printf( "Ply: '%s'.\n", plies_str__a );
            printf( "...\n" );

            CC_FREE( plies_str__a );
        } // TODO :: DEBUG :: DELETE


        if ( !cc_ply_extend( &plies__t, &ply__t ) ) {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );
            return false;
        }

        is_first_ply = false;
    }


    { // TODO :: DEBUG :: DELETE
        char * plies_str__a = cc_ply_all_to_string__new( plies__t );

        cc_str_print( plies_str__a, NULL, 0, "Plies: '%s'.\n", 0, NULL );
        printf( "---\n" );

        CC_FREE( plies_str__a );
    } // TODO :: DEBUG :: DELETE


    *plies__o = plies__t; // Ownership transfer.
    // plies__t = NULL; // Not needed.

    return true;
}
