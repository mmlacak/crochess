// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse.h"

/**
    @file cc_parse.c
    @brief Functions separating a move (algebraic notation string) into list of enums, sub-strings.
*/


size_t cc_starts_with_ply_link_len( char const * restrict an_str )
{
    if ( !an_str ) return 0;

    char const * c = an_str;

    if ( *c == '~' ) return 1; // "~" plies

    if ( *c == '|' )
    {
        if ( *++c == '|' ) return 2; // "||" failed teleportation, oblation
        return 1; // "|" teleportation
    }

    if ( *c == '@' )
    {
        if ( *++c == '@' )
        {
            if ( *++c == '@' ) return 3; // "@@@" failed trance-journey, oblation
            return 2; // "@@" dual trance-journey, oblation
        }
        return 1; // "@" trance-journey
    }

    if ( *c == ';' )
        if ( *++c == ';' ) return 2; // ";;" Pawn-sacrifice

    return 0;
}


char const * cc_traverse_plies( char const * restrict an_str,
                                bool skip_or_stop_at )
{
    if ( !an_str ) return NULL;

    char const * str__w = an_str;

    if ( skip_or_stop_at )
        str__w += cc_starts_with_ply_link_len( str__w );
    else
        while ( ( *str__w != '\0' ) &&
                ( cc_starts_with_ply_link_len( str__w ) == 0 ) )
            ++str__w;

    return str__w;
}


bool cc_ply_iter( char const * restrict an_str,
                  char const ** restrict ply_first__io,
                  char const ** restrict ply_end__io )
{
    if ( !an_str ) return false;
    if ( !ply_first__io ) return false;
    if ( !ply_end__io ) return false;

    if ( !( *ply_first__io ) && !( *ply_end__io ) )
        *ply_first__io = an_str;
    else if ( ( *ply_first__io ) && ( *ply_end__io ) )
        *ply_first__io = cc_traverse_plies( *ply_end__io, false );
    else
        return false;

    *ply_end__io = cc_traverse_plies( *ply_first__io, true );
    *ply_end__io = cc_traverse_plies( *ply_end__io, false );

    if ( ( **ply_first__io == '\0' ) || ( *ply_end__io == *ply_first__io ) )
    {
        *ply_first__io = *ply_end__io = NULL;
        return false;
    }

    return true;
}
