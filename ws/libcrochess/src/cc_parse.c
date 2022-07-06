// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_parse.h"

/**
    @file cc_parse.c
    @brief Functions separating a move (algebraic notation string) into list of enums, sub-strings.
*/


bool cc_starting_ply_link( char const * restrict an_str,
                           CcPlyLinkEnum * restrict ple__o )
{
    if ( !an_str ) return false;

    char const * c = an_str;

    if ( *c == '~' )
    {
        *ple__o = CC_PLE_CascadingPly; // "~" plies
        return true;
    }

    if ( *c == '|' )
    {
        if ( *++c == '|' )
        {
            *ple__o = CC_PLE_FailedTeleportation; // "||" failed teleportation, oblation
            return true;
        }

        *ple__o = CC_PLE_Teleportation; // "|" teleportation
        return true;
    }

    if ( *c == '@' )
    {
        if ( *++c == '@' )
        {
            if ( *++c == '@' )
            {
                *ple__o = CC_PLE_FailedTranceJourney; // "@@@" failed trance-journey, oblation
                return true;
            }

            *ple__o = CC_PLE_DualTranceJourney; // "@@" dual trance-journey, oblation
            return true;
        }

        *ple__o = CC_PLE_TranceJourney; // "@" trance-journey
        return true;
    }

    if ( *c == ';' )
        if ( *++c == ';' )
        {
            *ple__o = CC_PLE_PawnSacrifice; // ";;" Pawn-sacrifice
            return true;
        }

    if ( *c != '\0' )
    {
        *ple__o = CC_PLE_StartingPly;
        return true;
    }

    return false;
}

size_t cc_ply_link_len( CcPlyLinkEnum ple )
{
    switch ( ple )
    {
        case CC_PLE_StartingPly : return 0; /* Just first ply, standalone or starting a cascade. */
        case CC_PLE_CascadingPly : return 1; /* Just one ply, continuing cascade. Corresponds to `~`. */
        case CC_PLE_Teleportation : return 1; /* Teleportation of piece. Corresponds to `|`. */
        case CC_PLE_FailedTeleportation : return 2; /* Failed teleportation, corresponds to `||`. */
        case CC_PLE_TranceJourney : return 1; /* Trance-journey, corresponds to `@`. */
        case CC_PLE_DualTranceJourney : return 2; /* Double trance-journey, corresponds to `@@`. */
        case CC_PLE_FailedTranceJourney : return 3; /* Failed trance-journey, corresponds to `@@@`. */
        case CC_PLE_PawnSacrifice : return 2; /* Pawn sacrifice, corresponds to `;;`. */
        default : return 0;
    }
}

char const * cc_next_ply_link( char const * restrict an_str )
{
    if ( !an_str ) return NULL;
    if ( *an_str == '\0' ) return NULL;

    char const * str__w = an_str;
    CcPlyLinkEnum ple = CC_PLE_StartingPly;

    if ( cc_starting_ply_link( str__w, &ple ) )
        str__w += cc_ply_link_len( ple );

    // Skip over everything before ply link.
    while ( cc_starting_ply_link( str__w, &ple ) &&
            ( ple == CC_PLE_StartingPly ) )
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
        *start__io = *end__io;
    else
        return false;

    *end__io = cc_next_ply_link( *start__io );

    if ( **start__io == '\0' ) // ( ( **start__io == '\0' ) || ( *end__io == *start__io ) )
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

    while ( !isalnum( *p )  ) ++p;

    if ( isupper( *p ) ) // <!> Useage of cc_is_piece_symbol() here is bug,
                         //     all other upper chars would end as Pawns.
        *piece_symbol__o = *p;
    else
        *piece_symbol__o = 'P';

    return cc_is_piece_symbol( *piece_symbol__o );
}


CcStepLinkEnum cc_starting_step_link( char const * restrict an_str )
{
    if ( !an_str ) return CC_SLE_Start;

    char const * c = an_str;

    if ( *c == '.' )
    {
        if ( *++c == '.' )
            return CC_SLE_Distant;

        return CC_SLE_Next;
    }

    if ( *c == '-' )
        return CC_SLE_Destination;

    if ( *c == ',' )
        return CC_SLE_Reposition;

    return CC_SLE_Start;
}

size_t cc_step_link_len( CcStepLinkEnum sle )
{
    switch ( sle )
    {
        case CC_SLE_Start : return 0; /* Position from which a piece started moving. */
        case CC_SLE_Reposition : return 1; /* In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
        case CC_SLE_Next : return 1; /* Step immediately following previous, separated by . (dot). */
        case CC_SLE_Distant : return 2; /* Step not immediately following previous, separated by .. (double-dot). */
        case CC_SLE_Destination : return 1; /* Step to destination field, separated by - (hyphen). */
        default : return 0;
    }
}

char const * cc_traverse_steps( char const * restrict an_str,
                                bool skip_over_link )
{
    if ( !an_str ) return NULL;

    char const * str__w = an_str;

    if ( skip_over_link )
        str__w += cc_step_link_len( cc_starting_step_link( str__w ) );
    else
        while ( ( *str__w != '\0' ) &&
                ( cc_starting_step_link( str__w ) ) == CC_SLE_Start )
            ++str__w;

    return str__w;
}

bool cc_step_iter( char const * restrict an_str,
                   char const ** restrict start__io,
                   char const ** restrict end__io )
{
    if ( !an_str ) return false;
    if ( !start__io ) return false;
    if ( !end__io ) return false;

    if ( !( *start__io ) && !( *end__io ) )
        *start__io = an_str;
    else if ( ( *start__io ) && ( *end__io ) )
        *start__io = cc_traverse_steps( *end__io, false );
    else
        return false;

    *end__io = cc_traverse_steps( *start__io, true );
    *end__io = cc_traverse_steps( *end__io, false );

    if ( ( **start__io == '\0' ) || ( *end__io == *start__io ) )
    {
        *start__io = *end__io = NULL;
        return false;
    }

    return true;
}
