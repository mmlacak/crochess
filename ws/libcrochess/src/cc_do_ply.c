// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_utils.h"
#include "cc_do_ply.h"


static bool _cc_fail_with_msg_invalid_ply_link( char const * ply_start_an,
                                                char const * ply_end_an,
                                                CcParseMsg ** parse_msgs__iod ) {
    char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid ply separator in ply '%s'.\n", ply_str__a );
    CC_FREE( ply_str__a );
    return false;
}

bool cc_do_ply( char const * ply_start_an,
                char const * ply_end_an,
                CcGame * game,
                CcPosDesc * before_ply_start__io,
                CcChessboard ** cb__io,
                CcParseMsg ** parse_msgs__iod ) {
    if ( !ply_start_an ) return false;
    if ( !ply_end_an ) return false;
    if ( !game ) return false;
    if ( !before_ply_start__io ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__iod ) return false;

    //
    // Ply link.

    // TODO :: DELETE :: after cc_parse_ply_link() is fixed
    //
    // CcPlyLinkEnum ple = CC_PLE_None;
    // if ( !CC_MAYBE_IS_TRUE( cc_parse_ply_link( ply_start_an, &ple ) ) )
    //     return _cc_fail_with_msg_invalid_ply_link( ply_start_an, ply_end_an, parse_msgs__iod );
    //
    // TODO :: DELETE :: after cc_parse_ply_link() is fixed
    CcPlyLinkEnum ple = cc_parse_ply_link( ply_start_an );
    if ( ple == CC_PLE_None )
        return _cc_fail_with_msg_invalid_ply_link( ply_start_an, ply_end_an, parse_msgs__iod );

    // if ( is_first_ply && ( ple == CC_PLE_None ) ) ple = CC_PLE_StartingPly; // TODO :: DELETE :: after cc_parse_ply_link() is fixed

    char const * c_str = ply_start_an + cc_ply_link_len( ple );

    if ( *c_str == '[' ) ++c_str;

    //
    // Piece symbol.

    char piece_symbol = ' ';

    if ( !cc_fetch_piece_symbol( c_str, &piece_symbol, true, true ) ) {
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid piece symbol '%c'.\n", piece_symbol );
        return false;
    }

    bool is_light = CC_GAME_STATUS_IS_LIGHT_TURN( game->status );
    CcPieceType piece_an = cc_piece_from_symbol( piece_symbol, is_light ); // Piece type should be correct, but color (owner) might not be, if it's not first ply.

    if ( !cc_piece_has_same_type( before_ply_start__io->piece, piece_an ) ) return false;



    return false;
}
