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


static void cc_add_msg_invalid_ply_link( char const * ply_start_an,
                                         char const * ply_end_an,
                                         CcParseMsg ** parse_msgs__iod ) {
    char * ply_str__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid ply separator in ply '%s'.\n", ply_str__a );
    CC_FREE( ply_str__a );
}

static bool cc_check_king_ply( CcChessboard * cb,
                               CcPieceEnum king,
                               CcPos * pos__o,
                               CcParseMsg ** parse_msgs__iod ) {
    if ( !cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, king, false, pos__o ) ) {
        char const * piece_str = cc_piece_as_string( king, true, true );
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s not found on chessboard.\n", piece_str );
        return false;
    }

    CcPos pos = *pos__o; // Temp var, as next iter will start from found position, but we don't want to change what was already found.

    if ( cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, king, false, &pos ) ) { // Check if it's only one King of the same color on chessboard.
        char const * piece_str = cc_piece_as_string( king, false, true );
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "More than one %s was found on chessboard.\n", piece_str );
        return false;
    }

    return true;
}

static bool cc_check_piece_can_be_activated( CcPieceEnum piece,
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

static bool cc_parse_ply( char const * ply_start_an,
                          char const * ply_end_an,
                          CcGame * game,
                          CcPosDesc * before_ply_start__io,
                          bool is_first_ply,
                          CcParsedPly ** ply__o,
                          CcChessboard ** cb__io,
                          CcParseMsg ** parse_msgs__iod ) {
    if ( !before_ply_start__io ) return false;
    if ( *ply__o ) return false;

    //
    // Ply link.

    CcParsedPlyLinkEnum ple = CC_PPLE_None;
    if ( !cc_parse_ply_link( ply_start_an, &ple ) ) {
        cc_add_msg_invalid_ply_link( ply_start_an, ply_end_an, parse_msgs__iod );
        return false;
    }

    if ( is_first_ply && ( ple == CC_PPLE_None ) ) ple = CC_PPLE_StartingPly;

    char const * c_str = ply_start_an + cc_ply_link_len( ple );

    if ( CC_CHAR_IS_PLY_GATHER_START( *c_str ) ) ++c_str; // Move past '['.

    //
    // Piece symbol.

    char piece_symbol = ' ';

    if ( !cc_fetch_piece_symbol( c_str, &piece_symbol, true, true ) ) {
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid piece symbol '%c'.\n", piece_symbol );
        return false;
    }

    bool is_light = CC_GAME_STATUS_IS_LIGHT_TURN( game->status );
    CcPieceEnum piece_an = cc_piece_from_symbol( piece_symbol, is_light ); // Piece type should be correct, but color (owner) might not be, if it's not first ply.

    if ( is_first_ply ) {
        before_ply_start__io->piece = piece_an; // Piece type and owner should be correct, on the first ply.
        // Position, and tag are generally not known at this time.

        if ( CC_PIECE_IS_KING( piece_an ) ) {
            CcPos pos = CC_POS_CAST_INVALID;

            if ( !cc_check_king_ply( *cb__io, piece_an, &pos, parse_msgs__iod ) )
                return false;

            before_ply_start__io->pos = pos;
            before_ply_start__io->tag = cc_chessboard_get_tag( *cb__io, pos.i, pos.j );
        }
    } else {
        if ( !cc_piece_has_same_type( piece_an, before_ply_start__io->piece ) ) {
            char const * piece_str = cc_piece_as_string( before_ply_start__io->piece, false, true );
            char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
            cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Found %s, expected '%c' from notation; in ply '%s'.\n", piece_str, piece_symbol, ply_an__a );
            CC_FREE( ply_an__a );
            return false;
        }

        CcPos pos = before_ply_start__io->pos;
        CcPieceEnum pe = cc_chessboard_get_piece( *cb__io, pos.i, pos.j );

        if ( pe != before_ply_start__io->piece ) {
            cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
            if ( !cc_pos_to_short_string( pos, &pos_c8 ) )  return false;

            char const * piece_str = cc_piece_as_string( pe, false, true );
            char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
            cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Found %s at %s, expected '%c' from notation; in ply '%s'.\n", piece_str, pos_c8, piece_symbol, ply_an__a );
            CC_FREE( ply_an__a );
            return false;
        }

        if ( !cc_check_piece_can_be_activated( piece_an, ply_start_an, ply_end_an, parse_msgs__iod ) ) // This is fine, color (owner) does not matter.
            return false;
    }

    if ( CC_CHAR_IS_PIECE_SYMBOL( *c_str ) ) ++c_str;

    //
    // Losing tag.

    CcLosingTagEnum lte = cc_parse_losing_tag( c_str );

    if ( !cc_check_losing_tag_for_piece( before_ply_start__io->piece, lte ) ) {
        char const * piece_str = cc_piece_as_string( before_ply_start__io->piece, true, true );
        char const * lte_str = cc_losing_tag_as_string( lte );
        char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot lose tag '%s'; in ply '%s'.\n", piece_str, lte_str, ply_an__a );
        CC_FREE( ply_an__a );
        return false;
    }

    c_str += cc_losing_tag_len( lte );

    //
    // Steps.

    CcParsedStep * steps__t = NULL;

    if ( !cc_parse_steps( c_str, ply_end_an, game, *before_ply_start__io, &steps__t, cb__io,
                          parse_msgs__iod ) ) {
        cc_parsed_step_free_all( &steps__t );
        return false;
    }

    //
    // Finding starting position, tag, if first ply.

    CcParsedStep * start = NULL;

    if ( is_first_ply ) {
        start = cc_parsed_step_find_start( steps__t );

        if ( start && cc_pos_is_valid( start->field ) ) {
            CcPos start_pos = start->field;
            CcPieceEnum pe = cc_chessboard_get_piece( *cb__io, start_pos.i, start_pos.j );

            if ( pe == before_ply_start__io->piece ) {
                before_ply_start__io->pos = start_pos;
                before_ply_start__io->tag = cc_chessboard_get_tag( *cb__io, start_pos.i, start_pos.j );
            } else {
                cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
                if ( !cc_pos_to_short_string( start_pos, &pos_c8 ) ) {
                    cc_parsed_step_free_all( &steps__t );
                    return false;
                }

                char const * piece_str = cc_piece_as_string( pe, false, true );
                char * ply_an__a = cc_str_copy__new( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Found %s at %s, expected '%c' from notation, in ply '%s'.\n", piece_str, pos_c8, piece_symbol, ply_an__a );
                CC_FREE( ply_an__a );

                cc_parsed_step_free_all( &steps__t );
                return false;
            }
        } else {
            // TODO :: find starting position, tag by reversing pathing on chessboard

            // TODO :: handle disambiguation, check it's congruent with found starting position
        }
    }

    //
    // Updating last destination, before change.

    CcParsedStep * destination = cc_parsed_step_find_destination( steps__t );

    if ( !destination ) {
        cc_parsed_step_free_all( &steps__t );
        return false;
    }

    CcPos dest = destination->field;
    CcPosDesc new_ply_start = CC_POS_DESC_CAST_INVALID;

    new_ply_start.piece = cc_chessboard_get_piece( *cb__io, dest.i, dest.j );
    new_ply_start.pos = dest;
    new_ply_start.tag = cc_chessboard_get_tag( *cb__io, dest.i, dest.j );

    //
    // TODO :: update cb__io

    //
    // Create a new ply, which takes ownership of steps (steps__t).

    *ply__o = cc_parsed_ply__new( ple, before_ply_start__io->piece, lte, &steps__t );
    if ( !*ply__o ) {
        cc_parsed_step_free_all( &steps__t );
        return false;
    }

    *before_ply_start__io = new_ply_start;

    return true;
}


bool cc_parse_plies( char const * move_an,
                     CcGame * game,
                     CcParsedPly ** plies__o,
                     CcParseMsg ** parse_msgs__iod ) {
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !plies__o || *plies__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate__new( game->chessboard );
    if ( !cb__a ) return false;

    CcParsedPly * plies__t = NULL;
    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;
    CcPosDesc before_ply_start = CC_POS_DESC_CAST_INVALID;
    bool is_first_ply = true;

    while ( cc_iter_ply( move_an, &ply_start_an, &ply_end_an ) ) {
        CcParsedPly * ply__t = NULL;

        if ( !cc_parse_ply( ply_start_an, ply_end_an, game, &before_ply_start, is_first_ply, &ply__t, &cb__a,
                            parse_msgs__iod ) ) {
            cc_parsed_ply_free_all( &ply__t );
            cc_parsed_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );

            printf( "!cc_parse_ply( ... )\n" ); // TODO :: DEBUG :: DELETE

            return false;
        }

        // TODO :: DEBUG :: DELETE
        //
        {
            char * plies_str__a = cc_parsed_ply_all_to_short_string__new( ply__t );

            // cc_str_print( plies_str__a, NULL, 0, "Ply: '%s'.\n", 0, NULL );
            printf( "Ply: '%s'.\n", plies_str__a );
            printf( "...\n" );

            CC_FREE( plies_str__a );
        }
        //
        // TODO :: DEBUG :: DELETE

        if ( !cc_parsed_ply_extend( &plies__t, &ply__t ) ) {
            cc_parsed_ply_free_all( &ply__t );
            cc_parsed_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );

            printf( "!cc_parsed_ply_extend( ... )\n" ); // TODO :: DEBUG :: DELETE

            return false;
        }

        is_first_ply = false;
    }



    // TODO :: DEBUG :: DELETE
    //
    {
        char * plies_str__a = cc_parsed_ply_all_to_short_string__new( plies__t );

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
