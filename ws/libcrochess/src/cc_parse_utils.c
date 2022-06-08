// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_utils.h"

/**
    @file cc_parse_utils.c
    @brief Helper functions to parse algebraic notation.
*/

bool cc_is_char_ply_gatherer( char c )
{
    return ( ( c == '[' ) || ( c == ']' ) );
}

size_t cc_ply_separator_len( char const * restrict an_str )
{
    if ( !an_str ) return 0;

    char const * c = an_str;

    if ( *c == '~' ) return 1; // plies

    if ( *c == '|' ) // teleportation
    {
        if ( *++c == '|' ) // failed teleportation
            return 2;

        return 1;
    }

    if ( *c == '@' ) // trance-journey
    {
        if ( *++c == '@' ) // dual trance-journey
        {
            if ( *++c == '@' ) // failed trance-journey
                return 3;

            return 2;
        }

        return 1;
    }

    if ( *c == ';' ) // not valid yet
    {
        if ( *++c == ';' ) // Pawn-sacrifice
            return 2;

        return 0; // definitely not valid
    }

    return 0;
}

char const * cc_traverse_ply_separators( char const * restrict an_str,
                                         bool skip_or_stop_at )
{
    if ( !an_str ) return NULL;

    char const * str__w = an_str;

    if ( skip_or_stop_at )
        while ( *str__w != '\0' )
        {
            size_t len = cc_ply_separator_len( str__w );

            if ( len > 0 )
                str__w += len;
            else
                break;
        }
    else
        while ( ( *str__w != '\0' ) &&
                ( cc_ply_separator_len( str__w ) == 0 ) )
            ++str__w;

    return str__w;
}


bool cc_ply_iter( char const * restrict an_str,
                  char const ** restrict first__io,
                  char const ** restrict end__io )
{
    if ( !an_str ) return false;
    if ( !first__io ) return false;
    if ( !end__io ) return false;

    if ( !( *first__io ) && !( *end__io ) )
        *first__io = an_str;
    else if ( ( *first__io ) && ( *end__io ) )
        *first__io = *end__io;
    else
        return false;

    *first__io = cc_traverse_ply_separators( *first__io, true ); // skip
    *end__io = cc_traverse_ply_separators( *first__io, false ); // stop-at

    if ( ( **first__io == '\0' ) || ( *end__io == *first__io ) )
    {
        *first__io = *end__io = NULL;
        return false;
    }

    return true;
}
