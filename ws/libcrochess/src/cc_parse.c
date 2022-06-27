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
