// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_parse_utils.h"


bool cc_parse_ply_link( char const * ply_an_str,
                        CcParsedPlyLinkEnum * ple__o ) {
    if ( !ply_an_str ) return false;
    if ( !ple__o ) return false;

    char const * c = ply_an_str;

    if ( *c == '~' ) {
        *ple__o = CC_PPLE_CascadingPly; // "~" plies
        return true;
    }

    if ( *c == '|' ) {
        if ( *++c == '|' ) {
            if ( *++c == '|' ) {
                *ple__o = CC_PPLE_TeleportationOblation; // "|||" failed teleportation, oblation
                return true;
            }

            *ple__o = CC_PPLE_TeleportationReemergence; // "||" failed teleportation, re-emergence
            return true;
        }

        *ple__o = CC_PPLE_Teleportation; // "|" teleportation
        return true;
    }

    if ( *c == '@' ) {
        if ( *++c == '@' ) {
            if ( *++c == '@' ) {
                *ple__o = CC_PPLE_FailedTranceJourney; // "@@@" failed trance-journey, oblation
                return true;
            }

            *ple__o = CC_PPLE_DualTranceJourney; // "@@" dual trance-journey, oblation
            return true;
        }

        *ple__o = CC_PPLE_TranceJourney; // "@" trance-journey
        return true;
    }

    if ( *c == ';' ) {
        if ( *++c == ';' ) {
            *ple__o = CC_PPLE_PawnSacrifice; // ";;" Pawn-sacrifice
            return true;
        }

        return false;
    }

    if ( *c == '"' ) {
        *ple__o = CC_PPLE_SenseJourney; // "\"" sense-journey
        return true;
    }

    if ( *c == '\'' ) {
        *ple__o = CC_PPLE_FailedSenseJourney; // "'" failed sense-journey, oblation
        return true;
    }

    if ( isgraph( *c ) ) {
        *ple__o = CC_PPLE_None;
        return true;
    }

    return false;
}

size_t cc_ply_link_len( CcParsedPlyLinkEnum ple ) {
    switch ( ple ) {
        case CC_PPLE_None : return 0; // Ply link not found, uninitialized, or error happened.
        case CC_PPLE_StartingPly : return 0; // Just first ply, standalone or starting a cascade.
        case CC_PPLE_CascadingPly : return 1; // Just one ply, continuing cascade. Corresponds to `~`.
        case CC_PPLE_Teleportation : return 1; // Teleportation of piece. Corresponds to `|`.
        case CC_PPLE_TeleportationReemergence : return 2; // Failed teleportation, re-emergence, corresponds to `||`.
        case CC_PPLE_TeleportationOblation : return 3; // Failed teleportation, oblation, corresponds to `|||`.
        case CC_PPLE_TranceJourney : return 1; // Trance-journey, corresponds to `@`.
        case CC_PPLE_DualTranceJourney : return 2; // Double trance-journey, corresponds to `@@`.
        case CC_PPLE_FailedTranceJourney : return 3; // Failed trance-journey, corresponds to `@@@`.
        case CC_PPLE_PawnSacrifice : return 2; // Pawn sacrifice, corresponds to `;;`.
        case CC_PPLE_SenseJourney : return 1; // Sense-journey, corresponds to `"`.
        case CC_PPLE_FailedSenseJourney : return 1; // Failed sense-journey, corresponds to `'`.

        default : return 0;
    }
}

char const * cc_next_ply_link( char const * ply_an_str ) {
    if ( !ply_an_str ) return NULL;
    if ( *ply_an_str == '\0' ) return NULL;

    // Skip over current ply link.
    CcParsedPlyLinkEnum ple = CC_PPLE_None;
    if ( !cc_parse_ply_link( ply_an_str, &ple ) ) return NULL;
    char const * str__w = ply_an_str + cc_ply_link_len( ple );

    // Skip over everything before next ply link.
    while ( cc_parse_ply_link( str__w, &ple ) && ( ple == CC_PPLE_None ) )
        ++str__w;

    return str__w;
}

bool cc_iter_ply( char const * move_an_str,
                  char const ** start__io,
                  char const ** end__io ) {
    if ( !move_an_str ) return false;
    if ( !start__io ) return false;
    if ( !end__io ) return false;

    char const * an_end = cc_str_end( move_an_str, NULL, CC_MAX_LEN_ZERO_TERMINATED );
    if ( !an_end ) return false;

    if ( ( *start__io && ( ( *start__io < move_an_str ) || ( an_end < *start__io ) ) ) ||
         ( *end__io && ( ( *end__io < move_an_str ) || ( an_end < *end__io ) ) ) )
        // <!> Must manually reset pointers, otherwise it might be wrong set passed (botched copy-pasta?).
        return false;

    if ( !( *start__io ) && !( *end__io ) )
        *start__io = move_an_str;
    else if ( ( *start__io ) && ( *end__io ) )
        *start__io = *end__io;
    else
        return false;

    *end__io = cc_next_ply_link( *start__io );

    if ( !*end__io ) {
        *start__io = *end__io = NULL;
        return false;
    }

    return true;
}


bool cc_fetch_piece_symbol( char const * piece_an_str,
                            char * piece_symbol__o,
                            bool default_to_pawn,
                            bool return_validity ) {
    if ( !piece_an_str ) return false;
    if ( !piece_symbol__o ) return false;

    char const * p = piece_an_str;

    if ( isupper( *p ) ) // <!> Usage of cc_piece_symbol_is_valid() here is bug,
                         //     all other upper chars would end as Pawns.
        *piece_symbol__o = *p;
    else
        *piece_symbol__o = default_to_pawn ? 'P'
                                           : ' ';

    return return_validity ? cc_piece_symbol_is_valid( *piece_symbol__o )
                           : true;
}

CcLosingTagEnum cc_parse_losing_tag( char const * lt_an_str ) {
    if ( !lt_an_str ) return CC_LTE_NoneLost;

    char const * c = lt_an_str;

    if ( *c == '=' ) {
        if ( *++c == '=' )
            return CC_LTE_DelayedPromotionLost; // "==" losing promotion
    } else if ( *c == ':' ) {
        if ( *++c == ':' )
            return CC_LTE_RushingTagLost; // "::" losing rushing
    } else if ( *c == '&' ) {
        if ( *++c == '&' )
            return CC_LTE_CastlingTagLost; // "&&" losing castling
    }

    return CC_LTE_NoneLost;
}

size_t cc_losing_tag_len( CcLosingTagEnum lte ) {
    switch ( lte ) {
        case CC_LTE_DelayedPromotionLost : return 2; /* Losing promotion, corresponds to == (dual equal sign). */
        case CC_LTE_RushingTagLost : return 2; /* Losing ability to rush, corresponds to :: (double-colon). */
        case CC_LTE_CastlingTagLost : return 2; /* Losing ability to castle, corresponds to && (double-ampersand). */

        default : return 0; /* Others are not losing tags. */
    }
}

bool cc_convert_coords( char const * pos_an_str, int * file__o, int * rank__o ) {
    if ( !pos_an_str ) return false;

    char const * p = pos_an_str;

    if ( islower( *p ) ) {
        if ( !file__o ) return false;

        *file__o = CC_CONVERT_FILE_CHAR_INTO_NUM( *p++ );
    } else
        *file__o = CC_INVALID_COORD;

    if ( isdigit( *p ) ) {
        char const * c = p + 1;

        if ( isdigit( *c ) ) ++c;
        if ( isdigit( *c ) ) return false; // max len of rank is 2

        if ( !rank__o ) return false;

        *rank__o = CC_CONVERT_RANK_STR_INTO_NUM( p );
    } else
        *rank__o = CC_INVALID_COORD;

    return true;
}

bool cc_convert_pos( char const * pos_an_str, CcPos * pos__o ) {
    if ( !pos__o ) return false;
    return cc_convert_coords( pos_an_str, &pos__o->i, &pos__o->j );
}

bool cc_parse_pos( char const * pos_an_str,
                   CcPos * pos__o,
                   char const ** pos_end__o ) {
    if ( !pos_an_str ) return false;
    if ( !pos__o ) return false;
    if ( !pos_end__o ) return false;
    if ( *pos_end__o ) return false;

    char const * start = pos_an_str; // Position, or disambiguation start.
    char const * end = NULL; // Position, or disambiguation end.
    char const * c = pos_an_str;

    if ( islower( *c ) ) {
        if ( isdigit( *++c ) ) { // {1}
            if ( isdigit( *++c ) ) ++c;

            if ( !isdigit( *c ) )
                end = c;
            else
                return false; // Max len of rank is 2.
        }
        else
            end = c; // c was incremented above, see {1}.
    } else if ( isdigit( *c ) ) {
        if ( isdigit( *++c ) ) ++c;

        if ( !isdigit( *c ) )
            end = c;
        else
            return false; // Max len of rank is 2.
    } else
        return false;

    if ( !end ) return false;

    // if ( CC_CHAR_IS_PLY_GATHER_END( *c ) ) ++c; // Isn't used after this, so ...

    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;

    size_t len = (size_t)( end - start );
    size_t copied = cc_str_copy( start, end, len, pos_c8, NULL, CC_MAX_LEN_STEP_POS_AN );

    if ( len != copied ) return false;

    CcPos pos = CC_POS_CAST_INVALID;

    if ( !cc_convert_pos( pos_c8, &pos ) ) return false;

    *pos__o = pos;
    *pos_end__o = end;

    return true;
}

char const * cc_skip_disambiguation( char const * pos_an_str ) {
    if ( !pos_an_str ) return NULL;

    char const * c = pos_an_str;

    if ( islower( *c ) ) {
        ++c;

        if ( isdigit( *c ) ) {
            ++c;

            if ( isdigit( *c ) ) ++c;

            if ( islower( *c ) ) {
                return c;
            } else if ( CC_CHAR_IS_STEP_SEPARATOR( *c ) ) {
                return c;
            }
        } else if ( islower( *c ) ) {
            return c;
        } else if ( CC_CHAR_IS_STEP_SEPARATOR( *c ) ) {
            return c;
        }
    } else if ( isdigit( *c ) ) {
        ++c;

        if ( isdigit( *c ) ) ++c;

        if ( islower( *c ) ) {
            return c;
        } else if ( CC_CHAR_IS_STEP_SEPARATOR( *c ) ) {
            return c;
        }
    }

    // Function returns valid pointer **only** if disambiguation has been skipped.
    return NULL; // Do not return an_str!
}

bool cc_ply_has_separated_steps( char const * ply_an_str,
                                 char const * ply_end,
                                 bool check_intermediate_steps,
                                 bool check_destination_step ) {
    if ( !ply_an_str ) return false;
    if ( !ply_end ) return false;

    // if ( cc_skip_disambiguation( ply_an_str ) ) return true;

    if ( !check_intermediate_steps && !check_destination_step ) return false;

    char const * c = ply_an_str;

    while ( *c != '\0' && c < ply_end ) {
        if ( check_intermediate_steps && *c == '.' ) return true;
        if ( check_destination_step && *c == '-' ) return true;

        ++c;
    }

    return false;
}

bool cc_parse_step_link( char const * step_an_str,
                         char const * ply_end,
                         CcParsedStepLinkEnum * sle__o ) {
    if ( !step_an_str ) return false;
    if ( !sle__o ) return false;

    char const * c = step_an_str;

    if ( *c == '.' ) {
        if ( *++c == '.' ) {
            *sle__o = CC_PSLE_Distant;
            return true;
        }

        *sle__o = CC_PSLE_Next;
        return true;
    } else if ( *c == '-' ) {
        *sle__o = CC_PSLE_Destination;
        return true;
    } else if ( *c == ',' ) {
        *sle__o = CC_PSLE_Reposition;
        return true;
    } else if ( isgraph( *c ) ) {
        if ( cc_ply_has_separated_steps( step_an_str, ply_end, true, true ) ) {
            *sle__o = CC_PSLE_Start;
            return true;
        } else if ( cc_skip_disambiguation( step_an_str ) ) {
            *sle__o = CC_PSLE_Start;
            return true;
        } else {
            *sle__o = CC_PSLE_JustDestination;
            return true;
        }
    }

    return false;
}

size_t cc_parsed_step_link_len( CcParsedStepLinkEnum sle ) {
    switch ( sle ) {
        case CC_PSLE_None : return 0; /* Step link not found, uninitialized, or error happened. */
        case CC_PSLE_Start : return 0; /* Position from which a piece started moving. */
        case CC_PSLE_Reposition : return 1; /* In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
        case CC_PSLE_Next : return 1; /* Step immediately following previous, separated by . (dot). */
        case CC_PSLE_Distant : return 2; /* Step not immediately following previous, separated by .. (double-dot). */
        case CC_PSLE_Destination : return 1; /* Step to destination field, separated by - (hyphen). */
        case CC_PSLE_JustDestination : return 0; /* Just destination field, no separators, no other steps, maybe disambiguation. */

        default : return 0;
    }
}

char const * cc_next_step_link( char const * step_an_str,
                                char const * ply_end ) {
    if ( !step_an_str ) return NULL;
    if ( *step_an_str == '\0' ) return NULL;
    if ( !ply_end ) return NULL;
    if ( step_an_str >= ply_end ) return NULL;

    CcParsedStepLinkEnum sle = CC_PSLE_None;
    if ( !cc_parse_step_link( step_an_str, ply_end, &sle ) ) return NULL;

    char const * str__w = step_an_str + cc_parsed_step_link_len( sle );

    // Skip over everything before next step link.
    do {
        if ( !cc_parse_step_link( str__w, ply_end, &sle ) ) return NULL;

        if ( ( sle == CC_PSLE_Start ) || ( sle == CC_PSLE_JustDestination ) )
            ++str__w;
        else
            break;
    } while ( str__w < ply_end );

    return str__w;
}

bool cc_iter_step( char const * ply_an_str,
                   char const * ply_end,
                   char const ** start__io,
                   char const ** end__io ) {
    if ( !ply_an_str ) return false;
    if ( !ply_end ) return false;
    if ( !start__io ) return false;
    if ( !end__io ) return false;

    if ( ( *start__io && ( ( *start__io < ply_an_str ) || ( ply_end < *start__io ) ) ) ||
            ( *end__io && ( ( *end__io < ply_an_str ) || ( ply_end < *end__io ) ) ) )
        // <!> Must manually reset pointers, otherwise it might be wrong set passed (botched copy-pasta?).
        return false;

    if ( !( *start__io ) && !( *end__io ) )
        *start__io = ply_an_str;
    else if ( ( *start__io ) && ( *end__io ) )
        *start__io = *end__io;
    else
        return false;

    *end__io = cc_skip_disambiguation( *start__io );

    if ( !*end__io )
        *end__io = cc_next_step_link( *start__io, ply_end );

    if ( !*end__io ) {
        *start__io = *end__io = NULL;
        return false;
    }

    return true;
}


CcParsedSideEffectEnum cc_parse_side_effect_type( char const * step_an_str,
                                                  bool * has_promotion_sign__o ) {
    if ( !step_an_str ) return CC_PSEE_None;
    if ( !has_promotion_sign__o ) return CC_PSEE_None;

    char const * c = step_an_str;

    if ( *c == '*' ) {
        return CC_PSEE_Capture;
    } else if ( *c == '<' ) {
        return CC_PSEE_Displacement;
    } else if ( *c == ':' ) {
        return CC_PSEE_EnPassant;
    } else if ( *c == '&' ) {
        return CC_PSEE_Castle;
    } else if ( *c == '=' ) {
        if ( isupper( *++c ) ) {
            *has_promotion_sign__o = true;
            return CC_PSEE_Promotion;
        } else
            return CC_PSEE_TagForPromotion;
    } else if ( *c == '%' ) {
        if ( *++c == '%' )
            return CC_PSEE_FailedConversion;

        return CC_PSEE_Conversion;
    } else if ( *c == '^' ) {
        return CC_PSEE_Transparency;
    } else if ( *c == '/' ) {
        return CC_PSEE_Divergence;
    } else if ( *c == '>' ) {
        return CC_PSEE_DemoteToPawn;
    } else if ( *c == '$' ) {
        if ( *++c == '$' ) {
            if ( *++c == '$' )
                return CC_PSEE_FailedResurrection;

            return CC_PSEE_ResurrectingOpponent;
        }

        return CC_PSEE_Resurrection;
    } else if ( isupper( *c ) ) {
        *has_promotion_sign__o = false;
        return CC_PSEE_Promotion; // Promotion without `=`.
    }

    return CC_PSEE_None;
}

size_t cc_parsed_side_effect_type_len( CcParsedSideEffectEnum see,
                                       bool has_promotion_sign ) {
    switch ( see ) {
        // case CC_PSEE_None :
        case CC_PSEE_Capture : return 1;
        case CC_PSEE_Displacement : return 1;
        case CC_PSEE_EnPassant : return 1;
        case CC_PSEE_Castle : return 1;
        case CC_PSEE_Promotion : return has_promotion_sign ? 1 : 0;
        case CC_PSEE_TagForPromotion : return 1;
        case CC_PSEE_Conversion : return 1;
        case CC_PSEE_FailedConversion : return 2;
        case CC_PSEE_Transparency : return 1;
        case CC_PSEE_Divergence : return 1;
        case CC_PSEE_DemoteToPawn : return 1;
        case CC_PSEE_Resurrection : return 1;
        case CC_PSEE_ResurrectingOpponent : return 2;
        case CC_PSEE_FailedResurrection : return 3;

        default : return 0;
    }
}
