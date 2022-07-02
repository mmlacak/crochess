// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_parse.h"

/**
    @file cc_parse.c
    @brief Functions separating a move (algebraic notation string) into list of enums, sub-strings.
*/


CcPlyLinkEnum cc_starting_ply_link( char const * restrict an_str )
{
    if ( !an_str ) return CC_PLE_StartingPly;

    char const * c = an_str;

    if ( *c == '~' ) return CC_PLE_CascadingPly; // "~" plies

    if ( *c == '|' )
    {
        if ( *++c == '|' ) return CC_PLE_FailedTeleportation; // "||" failed teleportation, oblation
        return CC_PLE_Teleportation; // "|" teleportation
    }

    if ( *c == '@' )
    {
        if ( *++c == '@' )
        {
            if ( *++c == '@' ) return CC_PLE_FailedTranceJourney; // "@@@" failed trance-journey, oblation
            return CC_PLE_DualTranceJourney; // "@@" dual trance-journey, oblation
        }
        return CC_PLE_TranceJourney; // "@" trance-journey
    }

    if ( *c == ';' )
        if ( *++c == ';' ) return CC_PLE_PawnSacrifice; // ";;" Pawn-sacrifice

    return CC_PLE_StartingPly;
}

size_t cc_ply_link_len( CcPlyLinkEnum ple )
{
    switch ( ple )
    {
        case CC_PLE_StartingPly : return 0; /**< Just first ply, standalone or starting a cascade. */
        case CC_PLE_CascadingPly : return 1; /**< Just one ply, continuing cascade. Corresponds to `~`. */
        case CC_PLE_Teleportation : return 1; /**< Teleportation of piece. Corresponds to `|`. */
        case CC_PLE_FailedTeleportation : return 2; /**< Failed teleportation, corresponds to `||`. */
        case CC_PLE_TranceJourney : return 1; /**< Trance-journey, corresponds to `@`. */
        case CC_PLE_DualTranceJourney : return 2; /**< Double trance-journey, corresponds to `@@`. */
        case CC_PLE_FailedTranceJourney : return 3; /**< Failed trance-journey, corresponds to `@@@`. */
        case CC_PLE_PawnSacrifice : return 2; /**< Pawn sacrifice, corresponds to `;;`. */
        default : return 0;
    }
}

char const * cc_traverse_plies( char const * restrict an_str,
                                bool skip_or_stop_at )
{
    if ( !an_str ) return NULL;

    char const * str__w = an_str;

    if ( skip_or_stop_at )
        str__w += cc_ply_link_len( cc_starting_ply_link( str__w ) );
    else
        while ( ( *str__w != '\0' ) &&
                ( cc_ply_link_len( cc_starting_ply_link( str__w ) ) == 0 ) )
            ++str__w;

    return str__w;
}

bool cc_ply_iter( char const * restrict an_str,
                  char const ** restrict start__io,
                  char const ** restrict end__io )
{
    if ( !an_str ) return false;
    if ( !start__io ) return false;
    if ( !end__io ) return false;

    if ( !( *start__io ) && !( *end__io ) )
        *start__io = an_str;
    else if ( ( *start__io ) && ( *end__io ) )
        *start__io = cc_traverse_plies( *end__io, false );
    else
        return false;

    *end__io = cc_traverse_plies( *start__io, true );
    *end__io = cc_traverse_plies( *end__io, false );

    if ( ( **start__io == '\0' ) || ( *end__io == *start__io ) )
    {
        *start__io = *end__io = NULL;
        return false;
    }

    return true;
}

bool cc_ply_piece_symbol( char const * restrict an_str,
                          char * restrict piece_symbol__o )
{
    if ( !an_str ) return false;
    if ( !piece_symbol__o ) return false;

    char const * p = an_str;

    p = cc_traverse_plies( p, true );
    if ( !p ) return false;

    if ( isupper( *p ) ) // <!> Useage of cc_is_piece_symbol() here is bug,
                         //     all other upper chars would end as Pawns.
        *piece_symbol__o = *p;
    else
        *piece_symbol__o = 'P';

    return cc_is_piece_symbol( *piece_symbol__o );
}
