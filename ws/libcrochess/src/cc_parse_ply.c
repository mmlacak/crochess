// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdio.h> // TEMP

#include "cc_str_utils.h" // TEMP

#include "cc_tag.h"

#include "cc_parse_utils.h"
#include "cc_parse_ply.h"


bool cc_parse_plies( char const * restrict move_an,
                     CcGame * restrict game,
                     CcPly ** restrict plies__o,
                     CcParseMsg ** restrict parse_msgs__io )
{
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !plies__o ) return false;
    if ( *plies__o ) return false;
    if ( !parse_msgs__io ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate__new( game->chessboard );

    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;

    while ( cc_ply_iter( move_an, &ply_start_an, &ply_end_an ) )
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
            cc_parse_msg_append_format_if( parse_msgs__io,
                                           CC_PMTE_Error,
                                           CC_MAX_LEN_ZERO_TERMINATED,
                                           "Invalid piece symbol '%c'.\n",
                                           piece_symbol );

            cc_chessboard_free_all( &cb__a );
            return false;
        }

        if ( CC_CHAR_IS_PIECE_SYMBOL( *c_str ) ) ++c_str;

        //
        // Losing tag.

        CcTagEnum lte_an = cc_starting_losing_tag( c_str );

        if ( lte_an != CC_TE_None )
            c_str += cc_losing_tag_len( lte_an );



    }


    return true;
}
