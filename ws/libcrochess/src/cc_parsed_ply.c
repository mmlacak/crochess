// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_parsed_ply.h"


char const * cc_parsed_ply_link_symbol( CcParsedPlyLinkEnum ple ) {
    switch ( ple ) {
        case CC_PPLE_None : return NULL;
        case CC_PPLE_StartingPly : return "";
        case CC_PPLE_CascadingPly : return "~";
        case CC_PPLE_Teleportation : return "|";
        case CC_PPLE_TeleportationReemergence : return "||";
        case CC_PPLE_TeleportationOblation : return "|||";
        case CC_PPLE_TranceJourney : return "@";
        case CC_PPLE_DualTranceJourney : return "@@";
        case CC_PPLE_FailedTranceJourney : return "@@@";
        case CC_PPLE_PawnSacrifice : return ";;";
        case CC_PPLE_SenseJourney : return "\"";
        case CC_PPLE_FailedSenseJourney : return "'";

        default : return NULL;
    }
}


CcParsedPly * cc_parsed_ply__new( CcParsedPlyLinkEnum link,
                                  CcPieceType piece,
                                  CcLosingTagEnum lost_tag,
                                  CcParsedStep ** steps__n ) {
    CcParsedPly * ply__a = malloc( sizeof( CcParsedPly ) );
    if ( !ply__a ) return NULL;

    ply__a->link = link;
    ply__a->piece = piece;
    ply__a->lost_tag = lost_tag;

    if ( steps__n ) {
        ply__a->steps = *steps__n;
        *steps__n = NULL;
    } else
        ply__a->steps = NULL;

    ply__a->next = NULL;

    return ply__a;
}

CcParsedPly * cc_parsed_ply_append( CcParsedPly ** plies__iod_a,
                                    CcParsedPlyLinkEnum link,
                                    CcPieceType piece,
                                    CcLosingTagEnum lost_tag,
                                    CcParsedStep ** steps__n ) {
    if ( !plies__iod_a ) return NULL;

    CcParsedPly * ply__t = cc_parsed_ply__new( link, piece, lost_tag, steps__n );
    if ( !ply__t ) return NULL;

    if ( !*plies__iod_a ) {
        *plies__iod_a = ply__t; // Ownership transfer.
    } else {
        CcParsedPly * p = *plies__iod_a;
        CC_FASTFORWARD( p );
        p->next = ply__t; // Append + ownership transfer.
    }

    return ply__t; // Weak pointer.
}

CcParsedPly * cc_parsed_ply_duplicate_all__new( CcParsedPly * plies ) {
    if ( !plies ) return NULL;

    CcParsedPly * ply__a = NULL;
    CcParsedPly * from = plies;

    do {
        CcParsedStep * steps__t = cc_parsed_step_duplicate_all__new( from->steps );
        if ( !steps__t ) {
            cc_parsed_ply_free_all( &ply__a );
            return NULL;
        }

        CcParsedPly * ply__w = cc_parsed_ply_append( &ply__a,
                                                     from->link,
                                                     from->piece,
                                                     from->lost_tag,
                                                     &steps__t );
        if ( !ply__w ) {
            cc_parsed_step_free_all( &steps__t ); // Failed append --> ownership not transferred ...
            cc_parsed_ply_free_all( &ply__a );
            return NULL;
        }

        from = from->next;
    }
    while ( from );

    return ply__a;
}

CcParsedPly * cc_parsed_ply_extend( CcParsedPly ** plies__iod_a,
                                    CcParsedPly ** plies__d_n ) {
    if ( !plies__iod_a ) return NULL;
    if ( !plies__d_n ) return NULL;

    if ( !*plies__d_n ) return *plies__iod_a;

    if ( !*plies__iod_a ) {
        // Ownership transfer.
        *plies__iod_a = *plies__d_n;
        *plies__d_n = NULL;

        return *plies__iod_a;
    }

    CcParsedPly * last = *plies__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *plies__d_n;
    *plies__d_n = NULL;

    return last->next;
}

bool cc_parsed_ply_free_all( CcParsedPly ** plies__f ) {
    if ( !plies__f ) return false;
    if ( !*plies__f ) return true;

    bool result = true;
    CcParsedPly * ply = *plies__f;

    while ( ply ) {
        CcParsedStep ** steps = &( ply->steps );
        result = cc_parsed_step_free_all( steps ) && result;

        CcParsedPly * tmp = ply->next;
        CC_FREE( ply );
        ply = tmp;
    }

    *plies__f = NULL;
    return result;
}


size_t cc_parsed_ply_steps_count( CcParsedPly * ply ) {
    if ( !ply ) return 0;
    if ( !ply->steps ) return 0;

    size_t count = 0;
    CcParsedStep * s = ply->steps;

    while ( s ) {
        ++count;
        s = s->next;
    }

    return count;
}

bool cc_parsed_ply_contains_side_effects( CcParsedPly * ply ) {
    if ( !ply ) return false;
    if ( !ply->steps ) return false;

    CcParsedStep * s = ply->steps;
    while ( s->next ) {
        if ( s->side_effect.type != CC_PSEE_None ) return true;
        s = s->next;
    }

    return false;
}

CcPieceType cc_parsed_ply_find_activator( CcParsedPly * plies,
                                          CcParsedPly * ply__d ) {
    if ( !plies ) return CC_PE_None;

    if ( plies == ply__d ) // First ply in a linked list.
        return CC_PIECE_IS_ACTIVE( plies->piece ) ? plies->piece
                                                  : CC_PE_None;

    // <!> Shadows issue if ply is not contained in plies.
    //
    // if ( ply__d && CC_PIECE_IS_ACTIVE( ply__d->piece ) )
    //     return ply__d->piece;

    CcPieceType activator = CC_PE_None;
    bool ply_encountered = false;
    CcParsedPly * p = plies;

    while ( p ) {
        if ( CC_PIECE_IS_ACTIVE( p->piece ) )
            activator = p->piece;

        if ( p == ply__d ) {
            ply_encountered = true;
            break;
        }

        p = p->next;
    }

    if ( ply__d && !ply_encountered )
        return CC_PE_None;
    else
        return activator;
}

char * cc_parsed_ply_all_to_string__new( CcParsedPly * plies ) {
    if ( !plies ) return NULL;

    //
    // Count plies, steps.

    CcParsedPly * p_count = plies;
    size_t count_plies = 0;
    size_t count_steps = 0;

    while ( p_count ) {
        ++count_plies;
        count_steps += cc_parsed_step_count( p_count->steps );

        p_count = p_count->next;
    }

    //
    // Calc max string size, allocate.

    size_t step_size = CC_MAX_LEN_CHAR_8 + CC_MAX_LEN_CHAR_16 + 2;
                       // CC_MAX_LEN_CHAR_8, for position
                       // + CC_MAX_LEN_CHAR_16, for side-effect
                       // + 2, for step links, e.g. ".." before step

    size_t unused_size = ( count_plies * CC_MAX_LEN_PARSED_PLY_LINK_SYMBOL )
                       + ( count_steps * step_size )
                       + 1; // +1, for '\0'

    char * plies_str__a = calloc( unused_size, sizeof( char ) );
    if ( !plies_str__a ) return NULL;

    //
    // Collect ply string, append to result.

    CcParsedPly * p = plies;
    char * s = plies_str__a;

    while ( p ) {
        // Append ply link symbol.

        char const * pl = cc_parsed_ply_link_symbol( p->link );
        char * end_ple = cc_str_append_into( s, unused_size, pl, CC_MAX_LEN_PARSED_PLY_LINK_SYMBOL );

        if ( !end_ple ) {
            CC_FREE( plies_str__a );
            return NULL;
        }

        unused_size -= ( end_ple - s );
        s = end_ple;

        // Append piece symbol, lost tag.

        char piece_symbol = cc_piece_symbol( p->piece );
        *s++ = piece_symbol;

        char const * lte_str = cc_losing_tag_symbol( p->lost_tag );
        char * end_lte = cc_str_append_into( s, unused_size, lte_str, CC_MAX_LEN_LOSING_TAG );

        if ( !lte_str ) {
            CC_FREE( plies_str__a );
            return NULL;
        }

        unused_size -= ( end_lte - s );
        s = end_lte;

        // Append steps.

        char * steps_str__a = cc_parsed_step_all_to_string__new( p->steps );

        if ( !steps_str__a ) {
            CC_FREE( plies_str__a );
            return NULL;
        }

        char * end_steps = cc_str_append_into( s, unused_size, steps_str__a, CC_MAX_LEN_ZERO_TERMINATED );

        if ( !end_steps ) {
            CC_FREE( steps_str__a );
            CC_FREE( plies_str__a );
            return NULL;
        }

        unused_size -= ( end_steps - s );
        s = end_steps;

        CC_FREE( steps_str__a );
        p = p->next;
    }

    return plies_str__a;
}
