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

static bool _cc_check_king_ply( CcChessboard * cb,
                                CcPieceType king,
                                CcPos * pos__o,
                                CcParseMsg ** parse_msgs__iod ) {
    if ( !cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, king, false, pos__o ) ) {
        char const * piece_str = cc_piece_as_string( king, true, true );
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s not found on chessboard.\n", piece_str );
        return false;
    }

    CcPos pos = *pos__o; // Temp var, as next iter will init_step from found position, but we don't want to change what was already found.

    if ( cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, king, false, &pos ) ) { // Check if it's only one King of the same color on chessboard.
        char const * piece_str = cc_piece_as_string( king, false, true );
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "More than one %s was found on chessboard.\n", piece_str );
        return false;
    }

    return true;
}

static bool _cc_fail_with_msg_unexpected_piece_type( CcPieceType piece,
                                                     char piece_symbol,
                                                     char const * ply_start_an,
                                                     char const * ply_end_an,
                                                     CcParseMsg ** parse_msgs__iod ) {
    char const * piece_str = cc_piece_as_string( piece, false, true );
    char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Found %s, expected '%c' from notation; in ply '%s'.\n", piece_str, piece_symbol, ply_an__a );
    CC_FREE( ply_an__a );
    return false;
}

static bool _cc_fail_with_msg_unexpected_piece( CcPos pos,
                                                char piece_symbol,
                                                CcPieceType piece,
                                                char const * ply_start_an,
                                                char const * ply_end_an,
                                                CcParseMsg ** parse_msgs__iod ) {
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    if ( !cc_pos_to_string( pos, &pos_c8 ) ) return false;

    char const * piece_str = cc_piece_as_string( piece, false, true );
    char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Found %s at %s, expected '%c' from notation; in ply '%s'.\n", piece_str, pos_c8, piece_symbol, ply_an__a );
    CC_FREE( ply_an__a );
    return false;
}

static bool _cc_check_piece_can_be_activated( CcPieceType piece,
                                              char const * ply_start_an,
                                              char const * ply_end_an,
                                              CcParseMsg ** parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_ACTIVATED( piece ) ) {
        char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, true, true );
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot be activated, in ply '%s'.\n", piece_str, ply_str__a );
        CC_FREE( ply_str__a );
        return false;
    }

    return true;
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

static bool _cc_fail_with_msg_unexpected_pos( CcPos prev_dest,
                                              CcPos init_pos,
                                              char const * ply_start_an,
                                              char const * ply_end_an,
                                              CcParseMsg ** parse_msgs__iod ) {
    cc_char_8 prev_dest_c8 = CC_CHAR_8_EMPTY;
    if ( !cc_pos_to_string( prev_dest, &prev_dest_c8 ) ) return false;

    cc_char_8 init_pos_c8 = CC_CHAR_8_EMPTY;
    if ( !cc_pos_to_string( init_pos, &init_pos_c8 ) ) return false;

    char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Previous destination %s differs from initial position %s from notation; in ply '%s'.\n", prev_dest_c8, init_pos_c8, ply_an__a );
    CC_FREE( ply_an__a );
    return false;
}

static bool _cc_parse_ply( char const * ply_start_an,
                           char const * ply_end_an,
                           CcGame * game,
                           CcPosDesc * before_ply__io,
                           bool is_first_ply,
                           CcPly ** ply__o,
                           CcChessboard ** cb__io,
                           CcParseMsg ** parse_msgs__iod ) {
    if ( !before_ply__io ) return false;
    if ( *ply__o ) return false;

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

    bool is_light = ( game->status == CC_GSE_Turn_Light );
    CcPieceType pt_an = cc_piece_from_symbol( piece_symbol, is_light ); // Piece type should be correct, but color (owner) might not be, if activating opponent's pieces (if not first ply).

    if ( is_first_ply ) {
        before_ply__io->piece = pt_an; // Piece type and owner should be correct, on the first ply.
        // Position, and tag are generally not known at this time.

        if ( CC_PIECE_IS_KING( pt_an ) ) {
            CcPos pos = CC_POS_CAST_INVALID;

            if ( !_cc_check_king_ply( *cb__io, pt_an, &pos, parse_msgs__iod ) )
                return false;

            before_ply__io->pos = pos;
            before_ply__io->tag = cc_chessboard_get_tag( *cb__io, pos.i, pos.j );
        }
    } else {
        if ( !cc_piece_has_same_type( pt_an, before_ply__io->piece ) )
            return _cc_fail_with_msg_unexpected_piece( before_ply__io->pos, piece_symbol, before_ply__io->piece, ply_start_an, ply_end_an, parse_msgs__iod );

        if ( CC_PLY_LINK_TYPE_IS_ACTIVATING_PIECE( plte_an ) )
            if ( !_cc_check_piece_can_be_activated( pt_an, ply_start_an, ply_end_an, parse_msgs__iod ) ) // <!> This is fine, color (owner) does not matter.
                return false;
        // TODO :: handle other ply links

        // TODO :: check destination of previous ply == initial position in this ply
    }

    if ( cc_piece_symbol_is_valid( *c_an ) ) ++c_an;

    //
    // Losing tag.

    CcLosingTagType ltt_an = cc_parse_losing_tag( c_an );

    if ( !cc_check_piece_can_lose_tag( before_ply__io->piece, ltt_an ) )
        return _cc_fail_with_msg_piece_cannot_lose_tag( before_ply__io->piece, ltt_an, ply_start_an, ply_end_an, parse_msgs__iod );

    c_an += cc_losing_tag_len( ltt_an );

    //
    // Steps.

    CcStep * steps__t = NULL;

    if ( !cc_parse_steps( c_an, ply_end_an, game, *before_ply__io, &steps__t, cb__io,
                          parse_msgs__iod ) ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    //
    // Finding starting position, tag, if first ply.

    CcStep * init_step = cc_step_find_init( steps__t );

    if ( is_first_ply ) {
        if ( init_step && CC_POS_IS_VALID( init_step->field ) ) {
            CcPos init_pos = init_step->field;
            CcPieceType pt_cb = cc_chessboard_get_piece( *cb__io, init_pos.i, init_pos.j );

            if ( pt_cb == before_ply__io->piece ) {
                before_ply__io->pos = init_pos;
                before_ply__io->tag = cc_chessboard_get_tag( *cb__io, init_pos.i, init_pos.j );
            } else {
                cc_step_free_all( &steps__t );
                return _cc_fail_with_msg_unexpected_piece( init_pos, piece_symbol, pt_cb, ply_start_an, ply_end_an, parse_msgs__iod );
            }
        } else {
            // TODO :: find starting position, tag by reversing pathing on chessboard

            // TODO :: handle disambiguation, check it's congruent with found starting position
        }
    } else {
        if ( CC_PLY_LINK_TYPE_IS_ACTIVATING_PIECE( plte_an ) ) {
            if ( init_step ) {
                CcPos init_pos = init_step->field;
                CcPieceType pt_cb = cc_chessboard_get_piece( *cb__io, init_pos.i, init_pos.j );

                // TODO :: CHECK :: m Rb1-k1~Wj1-g7 :: m Rb1-j1~Wk1-g7 :: if CC_PLY_LINK_TYPE_IS_ACTIVATING_PIECE

                if ( !CC_POS_IS_EQUAL( init_pos, before_ply__io->pos ) )
                    return _cc_fail_with_msg_unexpected_pos( before_ply__io->pos, init_pos, ply_start_an, ply_end_an, parse_msgs__iod );
            }
        }
        // TODO :: find starting position, tag by reversing pathing on chessboard

        // TODO :: handle disambiguation, check it's congruent with found starting position
    }

    //
    // Updating last destination, before change.

    CcStep * destination = cc_step_find_destination( steps__t );

    if ( !destination ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    CcPos dest = destination->field;
    CcPosDesc before_this_ply = CC_POS_DESC_CAST_INVALID;

    before_this_ply.piece = cc_chessboard_get_piece( *cb__io, dest.i, dest.j );
    before_this_ply.pos = dest;
    before_this_ply.tag = cc_chessboard_get_tag( *cb__io, dest.i, dest.j );

    //
    // TODO :: update cb__io

    //
    // Create a new ply, which takes ownership of steps (steps__t).

    *ply__o = cc_ply__new( plte_an, before_ply__io->piece, ltt_an, &steps__t );
    if ( !*ply__o ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    *before_ply__io = before_this_ply;

    return true;
}


bool cc_parse_plies( char const * move_an,
                     CcGame * game,
                     CcPly ** plies__o,
                     CcParseMsg ** parse_msgs__iod ) {
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !plies__o || *plies__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate__new( game->chessboard );
    if ( !cb__a ) return false;

    CcPly * plies__t = NULL;
    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;
    CcPosDesc before_ply = CC_POS_DESC_CAST_INVALID;
    bool is_first_ply = true;

    while ( cc_iter_ply( move_an, &ply_start_an, &ply_end_an ) ) {
        CcPly * ply__t = NULL;

        if ( !_cc_parse_ply( ply_start_an, ply_end_an, game, &before_ply, is_first_ply, &ply__t, &cb__a,
                             parse_msgs__iod ) ) {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );

            printf( "!_cc_parse_ply( ... )\n" ); // TODO :: DEBUG :: DELETE

            return false;
        }

        // TODO :: DEBUG :: DELETE
        //
        {
            char * plies_str__a = cc_ply_all_to_string__new( ply__t );

            // cc_str_print( plies_str__a, NULL, 0, "Ply: '%s'.\n", 0, NULL );
            printf( "Ply: '%s'.\n", plies_str__a );
            printf( "...\n" );

            CC_FREE( plies_str__a );
        }
        //
        // TODO :: DEBUG :: DELETE

        if ( !cc_ply_extend( &plies__t, &ply__t ) ) {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );
            return false;
        }

        is_first_ply = false;
    }



    // TODO :: DEBUG :: DELETE
    //
    {
        char * plies_str__a = cc_ply_all_to_string__new( plies__t );

        cc_str_print( plies_str__a, NULL, 0, "Plies: '%s'.\n", 0, NULL );
        printf( "---\n" );

        CC_FREE( plies_str__a );
    }
    //
    // TODO :: DEBUG :: DELETE



    *plies__o = plies__t; // Ownership transfer.
    // plies__t = NULL; // Not needed.

    cc_chessboard_free_all( &cb__a );

    return true;
}
