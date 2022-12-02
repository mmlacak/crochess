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
                          CcPos * restrict last_destination__io,
                          CcPly ** restrict plies__io,
                          CcChessboard ** restrict cb__io,
                          CcParseMsg ** restrict parse_msgs__iod )
{
    //
    // Ply link.

    CcPlyLinkEnum ple = cc_starting_ply_link( ply_start_an );
    char const * c_str = ply_start_an + cc_ply_link_len( ple );

    if ( CC_CHAR_IS_PLY_GATHER_START( *c_str ) ) ++c_str; // Move past '['.

    //
    // Piece symbol.

    char piece_symbol = ' ';

    if ( !cc_find_ply_piece_symbol( c_str, &piece_symbol ) )
    {
        cc_parse_msg_append_format_if( parse_msgs__iod,
                                       CC_PMTE_Error,
                                       CC_MAX_LEN_ZERO_TERMINATED,
                                       "Invalid piece symbol '%c'.\n",
                                       piece_symbol );
        return false;
    }

    if ( CC_CHAR_IS_PIECE_SYMBOL( *c_str ) ) ++c_str;

    //
    // Losing tag.

    CcTagEnum lte = cc_starting_losing_tag( c_str );

    c_str += cc_losing_tag_len( lte );

    //
    // Steps.



// TODO :: update last_destination__iod

// TODO :: update cb__io


    //
    // Append a ply.

    // if ( !cc_ply_append_if( plies__io,
    //                         ply_start_an,
    //                         ply_end_an,
    //                         CC_MAX_LEN_ZERO_TERMINATED,
    //                         ple,
    //                         /* piece */,
    //                         lte,
    //                         /* steps */ ) )
    //     return false;

    return true;
}


bool cc_parse_plies( CcGame * restrict game,
                     CcMove ** restrict move__io,
                     CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !game ) return false;

    if ( !move__io ) return false;
    if ( !*move__io ) return false;

    if ( !( *move__io )->notation ) return false;
    if ( ( *move__io )->plies ) return false;

    if ( !parse_msgs__iod ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate__new( game->chessboard );
    CcPly * plies__t = NULL;

    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;
    CcPos last_destination = CC_POS_CAST_INVALID;

    while ( cc_ply_iter( ( *move__io )->notation, &ply_start_an, &ply_end_an ) )
    {
        if ( !cc_parse_ply( ply_start_an, ply_end_an, game, &last_destination,
                            &plies__t,
                            &cb__a,
                            parse_msgs__iod ) )
        {
            cc_ply_free_all( &plies__t );
            cc_chessboard_free_all( &cb__a );
            return false;
        }


    }



    ( *move__io )->plies = plies__t; // Ownership transfer.
    // plies__t = NULL; // Not needed.

    cc_chessboard_free_all( &cb__a );
    return true;
}
