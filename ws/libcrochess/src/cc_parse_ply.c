// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdio.h> // TEMP

#include "cc_str_utils.h" // TEMP

#include "cc_tag.h"

#include "cc_parse_utils.h"
#include "cc_parse_step.h"
#include "cc_parse_ply.h"


static bool cc_parse_ply( char const * restrict ply_start_an,
                          char const * restrict ply_end_an,
                          CcGame * restrict game,
                          CcPosPieceTag * restrict last_destination__io,
                          CcPly ** restrict ply__o,
                          CcChessboard ** restrict cb__io,
                          CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !last_destination__io ) return false;

    //
    // Ply link.

    CcPlyLinkEnum ple = cc_parse_ply_link( ply_start_an );
    char const * c_str = ply_start_an + cc_ply_link_len( ple );

    if ( CC_CHAR_IS_PLY_GATHER_START( *c_str ) ) ++c_str; // Move past '['.

    //
    // Piece symbol.

    char piece_symbol = ' ';

    if ( !cc_fetch_piece_symbol( c_str, &piece_symbol, true, true ) )
    {
        cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                    CC_PMTE_Error,
                                    CC_MAX_LEN_ZERO_TERMINATED,
                                    "Invalid piece symbol '%c'.\n",
                                    piece_symbol );
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

    if ( !cc_parse_steps( c_str, ply_end_an, game, *last_destination__io,
                          &steps__t,
                          cb__io,
                          parse_msgs__iod ) )
    {
        cc_step_free_all( &steps__t );
        return false;
    }

    //
    // Updating last destination, before change.

    CcStep * destination = cc_step_find_destination( steps__t );

    if ( !destination )
    {
        cc_step_free_all( &steps__t );
        return false;
    }

    CcPos pos = destination->field;

    last_destination__io->piece = cc_chessboard_get_piece( *cb__io, pos.i, pos.j );
    last_destination__io->pos = pos;
    last_destination__io->tag = cc_chessboard_get_tag( *cb__io, pos.i, pos.j );


// TODO :: update cb__io


    //
    // Append a ply.

    // if ( !cc_ply_append_if( plies__o,
    //                         ply_start_an,
    //                         ply_end_an,
    //                         CC_MAX_LEN_ZERO_TERMINATED,
    //                         ple,
    //                         /* piece */,
    //                         lte,
    //                         &steps__t ) )
    //     return false;

    return true;
}


bool cc_parse_plies( char const * restrict move_an,
                     CcGame * restrict game,
                     CcPly ** restrict plies__o,
                     CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !plies__o || *plies__o ) return false;
    if ( !parse_msgs__iod ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate__new( game->chessboard );
    CcPly * plies__t = NULL;

    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;
    CcPosPieceTag last_destination = CC_POS_PIECE_TAG_CAST_INVALID;

    while ( cc_iter_ply( move_an, &ply_start_an, &ply_end_an ) )
    {
        CcPly * ply__t = NULL;

        if ( !cc_parse_ply( ply_start_an, ply_end_an, game, &last_destination,
                            &ply__t,
                            &cb__a,
                            parse_msgs__iod ) )
        {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );
printf( "!cc_parse_ply( ... )\n" ); // TODO :: DEBUG :: DELETE
            return false;
        }

// TODO :: DEBUG :: DELETE
//
    // {
    //     char * plies_str__a = cc_ply_all_to_short_string__new( ply__t );

    //     // cc_str_print( plies_str__a, NULL, 0, "Ply: '%s'.\n", 0, NULL );
    //     printf( "Ply: '%s'.\n", plies_str__a );

    //     CC_FREE( plies_str__a );
    // }
//
// TODO :: DEBUG :: DELETE

        if ( !cc_ply_extend_if( &plies__t, &ply__t ) )
        {
            cc_ply_free_all( &ply__t );
            cc_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );
printf( "!cc_ply_extend_if( ... )\n" ); // TODO :: DEBUG :: DELETE
            return false;
        }
    }



// TODO :: DEBUG :: DELETE
//
    // {
    //     char * plies_str__a = cc_ply_all_to_short_string__new( plies__t );

    //     cc_str_print( plies_str__a, NULL, 0, "Plies: '%s'.\n", 0, NULL );

    //     CC_FREE( plies_str__a );
    // }
//
// TODO :: DEBUG :: DELETE



    *plies__o = plies__t; // Ownership transfer.
    // plies__t = NULL; // Not needed.

    cc_chessboard_free_all( &cb__a );
    return true;
}
