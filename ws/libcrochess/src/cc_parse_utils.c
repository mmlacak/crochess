// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_parse_utils.h"

/**
    @file cc_parse_utils.c
    @brief Functions separating a move (algebraic notation string) into list of enums, sub-strings.
*/


CcPlyLinkEnum cc_starting_ply_link( char const * restrict an_str )
{
    if ( !an_str ) return CC_PLE_None;

    char const * c = an_str;

    if ( *c == '~' )
        return CC_PLE_CascadingPly; // "~" plies

    if ( *c == '|' )
    {
        if ( *++c == '|' )
            return CC_PLE_FailedTeleportation; // "||" failed teleportation, oblation

        return CC_PLE_Teleportation; // "|" teleportation
    }

    if ( *c == '@' )
    {
        if ( *++c == '@' )
        {
            if ( *++c == '@' )
                return CC_PLE_FailedTranceJourney; // "@@@" failed trance-journey, oblation

            return CC_PLE_DualTranceJourney; // "@@" dual trance-journey, oblation
        }

        return CC_PLE_TranceJourney; // "@" trance-journey
    }

    if ( *c == ';' )
        if ( *++c == ';' )
            return CC_PLE_PawnSacrifice; // ";;" Pawn-sacrifice

    if ( *c != '\0' )
        return CC_PLE_StartingPly;

    return CC_PLE_None;
}

size_t cc_ply_link_len( CcPlyLinkEnum ple )
{
    switch ( ple )
    {
        case CC_PLE_None : return 0; /**< Ply link not found, uninitialized, or error happened. */
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

    // Skip over current ply link.
    CcPlyLinkEnum ple = cc_starting_ply_link( an_str );
    char const * str__w = an_str + cc_ply_link_len( ple );

    // Skip over everything before next ply link.
    while ( cc_starting_ply_link( str__w ) == CC_PLE_StartingPly )
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

    char const * an_end = cc_str_end( an_str, NULL, CC_MAX_LEN_ZERO_TERMINATED );
    if ( !an_end ) return false;

    if ( ( *start__io && ( ( *start__io < an_str ) || ( an_end < *start__io ) ) ) ||
         ( *end__io && ( ( *end__io < an_str ) || ( an_end < *end__io ) ) ) )
        // <!> Must manually reset pointers, otherwise it might be wrong set passed (botched copy-pasta?).
        return false;

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


bool cc_find_piece_symbol( char const * restrict an_str,
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

CcTagEnum cc_starting_losing_tag( char const * restrict an_str )
{
    if ( !an_str ) return CC_TE_None;

    char const * c = an_str;

    if ( *c == '=' )
    {
        if ( *++c == '=' )
            return CC_TE_DelayedPromotion; // "==" losing promotion
    }
    else if ( *c == ':' )
    {
        if ( *++c == ':' )
            return CC_TE_CanRush; // "::" losing rushing
    }
    else if ( *c == '&' )
    {
        if ( *++c == '&' )
            return CC_TE_CanCastle; // "&&" losing castling
    }

    return CC_TE_None;
}

size_t cc_losing_tag_len( CcTagEnum lte )
{
    switch ( lte )
    {
        case CC_TE_DelayedPromotion : return 2; /* Losing promotion, corresponds to == (dual equal sign). */
        case CC_TE_CanRush : return 2; /* Losing ability to rush, corresponds to :: (double-colon). */
        case CC_TE_CanCastle : return 2; /* Losing ability to castle, corresponds to && (double-ampersand). */

        default : return 0; // Others are not losing tags.
    }
}

bool cc_convert_starting_coords( char const * restrict pos,
                                 int * restrict file__o,
                                 int * restrict rank__o )
{
    if ( !pos ) return false;

    char const * p = pos;

    if ( islower( *p ) )
    {
        if ( !file__o ) return false;

        *file__o = CC_CONVERT_FILE_CHAR_INTO_NUM( *p++ );
    }
    else
        *file__o = CC_INVALID_COORD;

    if ( isdigit( *p ) )
    {
        char const * c = p + 1;

        if ( isdigit( *c ) ) ++c;
        if ( isdigit( *c ) ) return false; // max len of rank is 2

        if ( !rank__o ) return false;

        *rank__o = CC_CONVERT_RANK_STR_INTO_NUM( p );
    }
    else
        *rank__o = CC_INVALID_COORD;

    return true;
}

bool cc_convert_starting_pos( char const * restrict pos,
                              CcPos * restrict pos__o )
{
    return cc_convert_starting_coords( pos, &pos__o->i, &pos__o->j );
}

bool cc_starting_pos( char const * restrict an_str,
                      CcPos * restrict pos__o,
                      char const ** restrict pos_end__o )
{
    if ( !an_str ) return false;
    if ( !pos__o ) return false;
    if ( !pos_end__o ) return false;
    if ( *pos_end__o ) return false;

    char const * start = an_str; // Position, or disambiguation start.
    char const * end = NULL; // Position, or disambiguation end.
    char const * c = an_str;

    if ( islower( *c ) )
    {
        if ( isdigit( *++c ) ) // {1}
        {
            if ( isdigit( *++c ) ) ++c;

            if ( !isdigit( *c ) )
                end = c;
            else
                return false; // Max len of rank is 2.
        }
        else
            end = c; // c was incremented above, see {1}.
    }
    else if ( isdigit( *c ) )
    {
        if ( isdigit( *++c ) ) ++c;

        if ( !isdigit( *c ) )
            end = c;
        else
            return false; // Max len of rank is 2.
    }
    else
        return false;

    // if ( !end ) return false; // Should be fine.

    // if ( CC_CHAR_IS_PLY_GATHER_END( *c ) ) ++c; // Isn't used after this, so ...

    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;

    size_t len = (size_t)( end - start );
    size_t copied = cc_str_copy( start, end, len, pos_c8, CC_MAX_LEN_STEP_POS );

    if ( len != copied ) return false;

    CcPos pos = CC_POS_CAST_INVALID;

    if ( !cc_convert_starting_pos( pos_c8, &pos ) ) return false;

    *pos__o = pos;
    *pos_end__o = end;

    return true;
}


CcStepLinkEnum cc_starting_step_link( char const * restrict an_str )
{
    if ( !an_str ) return CC_SLE_None;

    char const * c = an_str;

    if ( *c == '.' )
    {
        if ( *++c == '.' )
            return CC_SLE_Distant;

        return CC_SLE_Next;
    }
    else if ( *c == '-' )
    {
        return CC_SLE_Destination;
    }
    else if ( *c == ',' )
    {
        return CC_SLE_Reposition;
    }
    else if ( *c != '\0' )
    {
        return CC_SLE_Start;
    }

    return CC_SLE_None;
}

size_t cc_step_link_len( CcStepLinkEnum sle )
{
    switch ( sle )
    {
        case CC_SLE_None : return 0; /**< Step link not found, uninitialized, or error happened. */
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
    if ( !ply_end ) return NULL;
    if ( an_str >= ply_end ) return NULL;

    CcStepLinkEnum sle = cc_starting_step_link( an_str );
    char const * str__w = an_str + cc_step_link_len( sle );

    // Skip over everything before step link.
    while ( ( cc_starting_step_link( str__w ) == CC_SLE_Start ) &&
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
    if ( !ply_end ) return false;
    if ( !start__io ) return false;
    if ( !end__io ) return false;

    if ( ( *start__io && ( ( *start__io < an_str ) || ( ply_end < *start__io ) ) ) ||
         ( *end__io && ( ( *end__io < an_str ) || ( ply_end < *end__io ) ) ) )
        // <!> Must manually reset pointers, otherwise it might be wrong set passed (botched copy-pasta?).
        return false;

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

    // Usually, step links are expected somewhere in the middle of AN string ...
    if ( ( an ) && ( an < ply_end ) ) return true;

    CcStepLinkEnum sle = cc_starting_step_link( an_str );

    // ... but string might start with step link.
    // If it's start of a ply AN, this is an error, but that needs handling somwhere else.
    return ( ( sle != CC_SLE_None ) && ( sle != CC_SLE_Start ) );
}


CcSideEffectEnum cc_starting_side_effect_type( char const * restrict an_str,
                                               bool * restrict has_promotion_sign__o )
{
    if ( !an_str ) return CC_SEE_None;
    if ( !has_promotion_sign__o ) return CC_SEE_None;

    char const * c = an_str;

    if ( *c == '*' )
    {
        return CC_SEE_Capture;
    }
    else if ( *c == '<' )
    {
        return CC_SEE_Displacement;
    }
    else if ( *c == ':' )
    {
        return CC_SEE_EnPassant;
    }
    else if ( *c == '&' )
    {
        return CC_SEE_Castle;
    }
    else if ( *c == '=' )
    {
        if ( isupper( *++c ) )
        {
            *has_promotion_sign__o = true;
            return CC_SEE_Promotion;
        }
        else
            return CC_SEE_TagForPromotion;
    }
    else if ( *c == '%' )
    {
        if ( *++c == '%' )
            return CC_SEE_FailedConversion;

        return CC_SEE_Conversion;
    }
    else if ( *c == '>' )
    {
        return CC_SEE_DemoteToPawn;
    }
    else if ( *c == '$' )
    {
        if ( *++c == '$' )
            return CC_SEE_FailedResurrection;

        return CC_SEE_Resurrection;
    }
    else if ( isupper( *c ) )
    {
        *has_promotion_sign__o = false;
        return CC_SEE_Promotion; // Promotion without `=`.
    }

    return CC_SEE_None;
}

size_t cc_side_effect_type_len( CcSideEffectEnum see,
                                bool has_promotion_sign )
{
    switch ( see )
    {
        // case CC_SEE_None :
        case CC_SEE_Capture : return 1;
        case CC_SEE_Displacement : return 1;
        case CC_SEE_EnPassant : return 1;
        case CC_SEE_Castle : return 1;
        case CC_SEE_Promotion : return has_promotion_sign ? 1 : 0;
        case CC_SEE_TagForPromotion : return 1;
        case CC_SEE_Conversion : return 1;
        case CC_SEE_FailedConversion : return 2;
        case CC_SEE_DemoteToPawn : return 1;
        case CC_SEE_Resurrection : return 1;
        case CC_SEE_FailedResurrection : return 2;

        default : return 0;
    }
}

// TODO :: DELETE
//
// char const * cc_find_side_effect_type( char const * restrict an_str,
//                                        char const * restrict step_end,
//                                        CcSideEffectEnum * restrict see__o )
// {
//     if ( !an_str ) return NULL;
//     if ( !step_end ) return NULL;

//     char const * c = an_str;
//     CcSideEffectEnum see = CC_SEE_None;
//     bool has_promotion_sign = true;

//     while ( c < step_end )
//     {
//         see = cc_starting_side_effect_type( c, has_promotion_sign );

//         if ( see != CC_SEE_None )
//         {
//             *see__o = see;
//             return c;
//         }

//         ++c;
//     }

//     return NULL;
// }
//
// TODO :: DELETE
