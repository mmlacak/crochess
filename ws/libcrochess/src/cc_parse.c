// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_str_utils.h"
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

    if ( !*end__io )
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


bool cc_starting_step_link( char const * restrict an_str,
                            CcStepLinkEnum * restrict sle__o )
{
    if ( !an_str ) return false;

    char const * c = an_str;

    if ( *c == '.' )
    {
        if ( *++c == '.' )
        {
            *sle__o = CC_SLE_Distant;
            return true;
        }

        *sle__o = CC_SLE_Next;
        return true;
    }

    if ( *c == '-' )
    {
        *sle__o = CC_SLE_Destination;
        return true;
    }

    if ( *c == ',' )
    {
        *sle__o = CC_SLE_Reposition;
        return true;
    }

    if ( *c != '\0' )
    {
        *sle__o = CC_SLE_Start;
        return true;
    }

    return false;
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

char const * cc_next_step_link( char const * restrict an_str,
                                char const * restrict ply_end )
{
    if ( !an_str ) return NULL;
    if ( *an_str == '\0' ) return NULL;
    if ( an_str >= ply_end ) return NULL;

    char const * str__w = an_str;
    CcStepLinkEnum sle = CC_SLE_Start;

    if ( cc_starting_step_link( str__w, &sle ) )
        str__w += cc_step_link_len( sle );

    // Skip over everything before step link.
    while ( cc_starting_step_link( str__w, &sle ) &&
            ( sle == CC_SLE_Start ) &&
            ( str__w < ply_end ) )
        ++str__w;

    return str__w;
}

bool cc_step_iter( char const * restrict an_str,
                   char const * restrict ply_end,
                   char const ** restrict start__io,
                   char const ** restrict end__io )
{
    if ( !an_str ) return false;
    if ( !start__io ) return false;
    if ( !end__io ) return false;

    if ( !( *start__io ) && !( *end__io ) )
        *start__io = an_str;
    else if ( *start__io )
        *start__io = *end__io;
    else
        return false;

    *end__io = cc_next_step_link( *start__io, ply_end );

    if ( !*end__io )
    {
        *start__io = *end__io = NULL;
        return false;
    }

    return true;
}

bool cc_ply_has_steps( char const * restrict an_str,
                       char const * restrict ply_end )
{
    char const * an = cc_next_step_link( an_str, ply_end );

    return ( ( an ) && ( an < ply_end ) );
}


char const * cc_starting_disambiguation( char const * restrict an_str,
                                         char const * restrict ply_end,
                                         char_8 * restrict disambiguation__o )
{
    if ( !an_str ) return NULL;
    if ( !ply_end ) return NULL;

    char const * step_end = cc_next_step_link( an_str, ply_end );
    if ( !step_end ) return NULL;

    if ( step_end == an_str ) return step_end;

    char const * start_da = NULL; // disambiguation start
    char const * end_da = NULL; // disambiguation end, aka true step start
    char const * c = an_str;
    char const * s = an_str;

    if ( islower( *c ) )
    {
        if ( islower( *++c ) )
        {
            s = c;

            if ( isdigit( *++c ) )
            {
                if ( isdigit( *++c ) ) ++c;
            }
            else
                return NULL;

            start_da = an_str;
            end_da = s;
        }
        else if ( isdigit( *c ) )
        {
            if ( isdigit( *++c ) ) ++c;

            if ( islower( *c ) )
            {
                s = c;

                if ( isdigit( *++c ) )
                {
                    if ( isdigit( *++c ) ) ++c;
                }
                else
                    return NULL;

                start_da = an_str;
                end_da = s;
            }
            else if ( ( *c != '\0' ) && ( c == step_end ) )
            {
                start_da = an_str;
                end_da = c;
            }
            else
                return NULL;
        }
        else if ( c == step_end )
        {
            start_da = an_str;
            end_da = c;
        }
        else
            return NULL;
    }
    else if ( isdigit( *c ) )
    {
        if ( isdigit( *++c ) ) ++c;

        if ( islower( *c ) )
        {
            s = c;

            if ( isdigit( *++c ) )
            {
                if ( isdigit( *++c ) ) ++c;
            }
            else
                return NULL;

            start_da = an_str;
            end_da = s;
        }
        else if ( c == step_end )
        {
            start_da = an_str;
            end_da = c;
        }
        else
            return NULL;
    }
    else
        return NULL;

    if ( !start_da ) return NULL;
    if ( !end_da ) return NULL;

    if ( CC_IS_PLY_GATHER_END( *c ) ) ++c;
    if ( c != step_end ) return NULL;

    if ( !cc_str_clear( *disambiguation__o, CC_MAX_LEN_CHAR_8 ) )
        return NULL;

    size_t len = (size_t)( end_da - start_da );
    size_t copied = cc_str_copy( start_da, end_da, len, *disambiguation__o, CC_MAX_LEN_DISAMBIGUATION );

    if ( len != copied )
        return NULL;

    return end_da;
}
