// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_parse_utils.h"
#include "cc_checks.h"

#include "cc_do_bishop.h"
#include "cc_do_ply.h"


static bool _cc_fail_with_msg_invalid_ply_link( char const * ply_start_an,
                                                char const * ply_end_an,
                                                CcParseMsg ** parse_msgs__iod ) {
    char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid ply separator in ply '%s'.\n", ply_str__a );
    CC_FREE( ply_str__a );
    return false;
}

static bool _cc_fail_with_msg_pieces_different_type( char const * ply_start_an,
                                                     char const * ply_end_an,
                                                     CcPosDesc before_ply,
                                                     CcPieceType an_piece,
                                                     CcParseMsg ** parse_msgs__iod ) {
    char const * piece_bps = cc_piece_as_string( before_ply.piece, false, false );
    char const * piece_str = cc_piece_as_string( an_piece, false, false );

    char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Piece from notation (%s) is different type from one on chessboard (%s); in ply '%s'.\n", piece_str, piece_bps, ply_str__a );
    CC_FREE( ply_str__a );

    return false;
}

static bool _cc_fail_with_msg_piece_cant_loose_tag( char const * ply_start_an,
                                                    char const * ply_end_an,
                                                    CcPieceType bp_piece,
                                                    CcTagType an_tt,
                                                    CcParseMsg ** parse_msgs__iod ) {
    char const * piece_str = cc_piece_as_string( bp_piece, true, true );
    char const * tag_str = cc_tag_as_string( an_tt, false, true );

    char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot loose %s tag; in ply '%s'.\n", piece_str, tag_str, ply_an__a );
    CC_FREE( ply_an__a );

    return false;
}

bool cc_do_ply( char const * ply_start_an,
                char const * ply_end_an,
                CcGame * game,
                CcPosDesc * before_ply__io,
                CcChessboard ** cb__io,
                CcParseMsg ** parse_msgs__iod ) {
    if ( !ply_start_an ) return false;
    if ( !ply_end_an ) return false;
    if ( !game ) return false;
    if ( !before_ply__io ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__iod ) return false;

    //
    // Ply link.

    CcPlyLinkEnum ple = cc_parse_ply_link( ply_start_an );
    if ( ple == CC_PLE_None )
        return _cc_fail_with_msg_invalid_ply_link( ply_start_an, ply_end_an, parse_msgs__iod );

    char const * c_str = ply_start_an + cc_ply_link_len( ple );

    if ( *c_str == '[' ) ++c_str; // Skip ply gather char.

    //
    // Piece symbol.

    char piece_symbol = ' ';

    if ( !cc_fetch_piece_symbol( c_str, &piece_symbol, true, true ) ) {
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid piece symbol '%c'.\n", piece_symbol );
        return false;
    }

    bool is_light = CC_GAME_STATUS_IS_LIGHT_TURN( game->status );
    CcPieceType an_piece = cc_piece_from_symbol( piece_symbol, is_light ); // [1] Piece type should be correct, but color (owner) might not be, if it's not first ply.

    if ( !cc_piece_has_same_type( before_ply__io->piece, an_piece ) ) // Compare previous piece at position vs. piece parsed from notation. Due to [1], colors might not be the same.
        return _cc_fail_with_msg_pieces_different_type( ply_start_an, ply_end_an, *before_ply__io, an_piece, parse_msgs__iod );

    if ( isupper( *c_str ) ) ++c_str; // Skip piece symbol.

    //
    // Losing tag.

    CcTagType an_tt = cc_parse_losing_tag( c_str );

    if ( !cc_check_losing_tag_for_piece( before_ply__io->piece, an_tt ) )
        return _cc_fail_with_msg_piece_cant_loose_tag( ply_start_an, ply_end_an, before_ply__io->piece, an_tt, parse_msgs__iod );

    c_str += cc_losing_tag_len( an_tt );

    //
    // TODO :: Check starting position.


    //
    // Dispatcher by piece.

    CcPieceType cb_piece = cc_chessboard_get_piece( game->chessboard, before_ply__io->pos.i, before_ply__io->pos.j );

    if ( CC_PIECE_IS_BISHOP( before_ply__io->piece ) ) {
        if ( !cc_do_bishop( c_str, ply_end_an, game, before_ply__io, cb__io, parse_msgs__iod ) )
            return false; // Error msgs are added within cc_do_bishop().
    }
    // else if ( CC_PIECE_IS_WAVE( before_ply__io->piece ) && CC_PIECE_IS_BISHOP( cb_piece ) )


    return false;
}
