// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdio.h> // TEMP

#include "cc_str_utils.h" // TEMP

#include "cc_tag.h"

#include "cc_path_defs.h"
#include "cc_path_gens.h"

#include "cc_parse_utils.h"
#include "cc_parse_step.h"
#include "cc_parse_ply.h"


static bool cc_check_ply_link_is_valid( CcPlyLinkEnum ple,
                                        char const * restrict ply_start_an,
                                        char const * restrict ply_end_an,
                                        bool is_first_ply,
                                        CcParseMsg ** restrict parse_msgs__iod ) {
    if ( ( ple == CC_PLE_None ) || ( is_first_ply && ( ple != CC_PLE_StartingPly ) ) ) {
        char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid ply link in ply '%s'.\n", ply_str__a );
        CC_FREE( ply_str__a );
        return false;
    }

    return true;
}

static bool cc_check_king_ply( CcChessboard * restrict cb,
                               CcPieceEnum piece,
                               CcPos * restrict pos__o,
                               CcParseMsg ** restrict parse_msgs__iod ) {
    bool is_light = cc_piece_is_light( piece );

    if ( !cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, piece, false, pos__o ) ) {
        char * color = is_light ? "Light" : "Dark";
        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s King not found.\n", color );
        return false;
    }

    CcPos pos = *pos__o;

    if ( cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, piece, false, &pos ) ) {
        char * color = is_light ? "light" : "dark";
        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "More than one %s King was found.\n", color );
        return false;
    }

    return true;
}

static bool cc_check_piece_can_be_activated( CcPieceEnum piece,
                                             char const * restrict ply_start_an,
                                             char const * restrict ply_end_an,
                                             CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_ACTIVATED( piece ) ) {
        char const * piece_str = cc_piece_label( piece );
        char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );

        if ( cc_piece_has_prefix( piece ) ) {
            char const * prefix = cc_piece_prefix( piece, true );
            cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s %s cannot be activated, in ply '%s'.\n", prefix, piece_str, ply_str__a );
        } else {
            cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot be activated, in ply '%s'.\n", piece_str, ply_str__a );
        }

        CC_FREE( ply_str__a );
        return false;
    }

    return true;
}

static bool cc_parse_ply( char const * restrict ply_start_an,
                          char const * restrict ply_end_an,
                          CcGame * restrict game,
                          CcPosPieceTag * restrict before_ply_start__io,
                          bool is_first_ply,
                          CcPly ** restrict ply__o,
                          CcChessboard ** restrict cb__io,
                          CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !before_ply_start__io ) return false;
    if ( *ply__o ) return false;

    //
    // Ply link.

    CcPlyLinkEnum ple = cc_parse_ply_link( ply_start_an );
    if ( !cc_check_ply_link_is_valid( ple, ply_start_an, ply_end_an, is_first_ply, parse_msgs__iod ) )
        return false;

    char const * c_str = ply_start_an + cc_ply_link_len( ple );

    if ( CC_CHAR_IS_PLY_GATHER_START( *c_str ) ) ++c_str; // Move past '['.

    //
    // Piece symbol.

    char piece_symbol = ' ';

    if ( !cc_fetch_piece_symbol( c_str, &piece_symbol, true, true ) ) {
        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid piece symbol '%c'.\n", piece_symbol );
        return false;
    }

    *before_ply_start__io = CC_POS_PIECE_TAG_CAST_INVALID;
    bool is_light = CC_GAME_STATUS_IS_LIGHT_TURN( game->status );
    CcPieceEnum piece = cc_piece_from_symbol( piece_symbol, is_light );

    if ( is_first_ply ) {
        before_ply_start__io->piece = piece;
        // Position, and tag are generaly not known at this time.

        if ( CC_PIECE_IS_KING( piece ) ) {
            CcPos pos = CC_POS_CAST_INVALID;

            if ( !cc_check_king_ply( *cb__io, piece, &pos, parse_msgs__iod ) )
                return false;

            before_ply_start__io->pos = pos;
            before_ply_start__io->tag = cc_chessboard_get_tag( *cb__io, pos.i, pos.j );
        }
    } else {
        if ( !cc_check_piece_can_be_activated( piece, ply_start_an, ply_end_an, parse_msgs__iod ) )
            return false;
    }

    if ( CC_CHAR_IS_PIECE_SYMBOL( *c_str ) ) ++c_str;

    //
    // Losing tag.

    CcLosingTagEnum lte = cc_parse_losing_tag( c_str );

    c_str += cc_losing_tag_len( lte );

    //
    // Steps.

    CcStep * steps__t = NULL;

    if ( !cc_parse_steps( c_str, ply_end_an, game, *before_ply_start__io, &steps__t, cb__io,
                          parse_msgs__iod ) ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    //
    // Updating last destination, before change.

    CcStep * destination = cc_step_find_destination( steps__t );

    if ( !destination ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    CcPos pos = destination->field;

    before_ply_start__io->piece = cc_chessboard_get_piece( *cb__io, pos.i, pos.j );
    before_ply_start__io->pos = pos;
    before_ply_start__io->tag = cc_chessboard_get_tag( *cb__io, pos.i, pos.j );

    *ply__o = cc_ply__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED, ple, piece, lte, &steps__t );
    if ( !*ply__o ) return false;


    // TODO :: update cb__io


    return true;
}


bool cc_parse_plies( char const * restrict move_an,
                     CcGame * restrict game,
                     CcPly ** restrict plies__o,
                     CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !plies__o || *plies__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate__new( game->chessboard );
    CcPly * plies__t = NULL;

    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;
    CcPosPieceTag before_ply_start = CC_POS_PIECE_TAG_CAST_INVALID;
    bool is_first_ply = true;

    while ( cc_iter_ply( move_an, &ply_start_an, &ply_end_an ) ) {
        CcPly * ply__t = NULL;

        if ( !cc_parse_ply( ply_start_an, ply_end_an, game, &before_ply_start, is_first_ply, &ply__t, &cb__a,
                            parse_msgs__iod ) ) {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );

            printf( "!cc_parse_ply( ... )\n" ); // TODO :: DEBUG :: DELETE

            return false;
        }

        // TODO :: DEBUG :: DELETE
        //
        {
            char * plies_str__a = cc_ply_all_to_short_string__new( ply__t );

            // cc_str_print( plies_str__a, NULL, 0, "Ply: '%s'.\n", 0, NULL );
            printf( "Ply: '%s'.\n", plies_str__a );
            printf( "...\n" );

            CC_FREE( plies_str__a );
        }
        //
        // TODO :: DEBUG :: DELETE

        if ( !cc_ply_extend_if( &plies__t, &ply__t ) ) {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );

            printf( "!cc_ply_extend_if( ... )\n" ); // TODO :: DEBUG :: DELETE

            return false;
        }

        is_first_ply = false;
    }



    // TODO :: DEBUG :: DELETE
    //
    {
        char * plies_str__a = cc_ply_all_to_short_string__new( plies__t );

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
