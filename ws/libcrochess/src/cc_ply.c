// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"


char const * cc_ply_link_type_symbol( CcPlyLinkTypeEnum plte ) {
    switch ( plte ) {
        case CC_PLTE_None : return NULL;
        case CC_PLTE_StartingPly : return "";
        case CC_PLTE_CascadingPly : return "~";
        case CC_PLTE_Teleportation : return "|";
        case CC_PLTE_TeleportationReemergence : return "||";
        case CC_PLTE_TeleportationOblation : return "|||";
        case CC_PLTE_TranceJourney : return "@";
        case CC_PLTE_DualTranceJourney : return "@@";
        case CC_PLTE_FailedTranceJourney : return "@@@";
        case CC_PLTE_PawnSacrifice : return ";;";
        case CC_PLTE_SenseJourney : return "\"";
        case CC_PLTE_FailedSenseJourney : return "'";

        default : return NULL;
    }
}


CcPly * cc_ply__new( CcPlyLinkTypeEnum link,
                     CcPieceTagType piece,
                     CcLosingTagType lost_tag,
                     CcStep ** steps__d_n ) {
    CcPly * ply__a = malloc( sizeof( CcPly ) );
    if ( !ply__a ) return NULL;

    ply__a->link = link;
    ply__a->piece = piece;
    ply__a->lost_tag = lost_tag;

    if ( steps__d_n ) {
        ply__a->steps = *steps__d_n;
        *steps__d_n = NULL;
    } else
        ply__a->steps = NULL;

    ply__a->next = NULL;

    return ply__a;
}

CcPly * cc_ply_append( CcPly ** plies__iod_a,
                       CcPlyLinkTypeEnum link,
                       CcPieceTagType piece,
                       CcLosingTagType lost_tag,
                       CcStep ** steps__d_n ) {
    if ( !plies__iod_a ) return NULL;

    CcPly * ply__t = cc_ply__new( link, piece, lost_tag, steps__d_n );
    if ( !ply__t ) return NULL;

    if ( !*plies__iod_a ) {
        *plies__iod_a = ply__t; // Ownership transfer.
    } else {
        CcPly * p = *plies__iod_a;
        CC_FASTFORWARD( p );
        p->next = ply__t; // Append + ownership transfer.
    }

    return ply__t; // Weak pointer.
}

CcPly * cc_ply_duplicate_all__new( CcPly * plies ) {
    if ( !plies ) return NULL;

    CcPly * ply__a = NULL;
    CcPly * from = plies;

    do {
        CcStep * steps__t = cc_step_duplicate_all__new( from->steps );
        if ( !steps__t ) {
            cc_ply_free_all( &ply__a );
            return NULL;
        }

        CcPly * ply__w = cc_ply_append( &ply__a,
                                                     from->link,
                                                     from->piece,
                                                     from->lost_tag,
                                                     &steps__t );
        if ( !ply__w ) {
            cc_step_free_all( &steps__t ); // Failed append --> ownership not transferred ...
            cc_ply_free_all( &ply__a );
            return NULL;
        }

        from = from->next;
    }
    while ( from );

    return ply__a;
}

CcPly * cc_ply_extend( CcPly ** plies__iod_a,
                       CcPly ** plies__d_n ) {
    if ( !plies__iod_a ) return NULL;
    if ( !plies__d_n ) return NULL;

    if ( !*plies__d_n ) return *plies__iod_a;

    if ( !*plies__iod_a ) {
        // Ownership transfer.
        *plies__iod_a = *plies__d_n;
        *plies__d_n = NULL;

        return *plies__iod_a;
    }

    CcPly * last = *plies__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *plies__d_n;
    *plies__d_n = NULL;

    return last->next;
}

bool cc_ply_free_all( CcPly ** plies__f ) {
    if ( !plies__f ) return false;
    if ( !*plies__f ) return true;

    bool result = true;
    CcPly * ply = *plies__f;

    while ( ply ) {
        CcStep ** steps = &( ply->steps );
        result = cc_step_free_all( steps ) && result;

        CcPly * tmp = ply->next;
        CC_FREE( ply );
        ply = tmp;
    }

    *plies__f = NULL;
    return result;
}


size_t cc_ply_steps_count( CcPly * ply ) {
    if ( !ply ) return 0;
    if ( !ply->steps ) return 0;

    size_t count = 0;
    CcStep * s = ply->steps;

    while ( s ) {
        ++count;
        s = s->next;
    }

    return count;
}

bool cc_ply_contains_side_effects( CcPly * ply ) {
    if ( !ply ) return false;
    if ( !ply->steps ) return false;

    CcStep * s = ply->steps;
    while ( s->next ) {
        if ( s->side_effect.type != CC_SETE_None ) return true;
        s = s->next;
    }

    return false;
}

CcPieceTagType cc_ply_find_activator( CcPly * plies, CcPly * ply__d ) {
    if ( !plies ) return CC_PTE_None;

    if ( !CC_PIECE_IS_VALID( plies->piece ) ) return CC_PTE_None;

    if ( plies == ply__d ) // First ply in a linked list.
        return CC_PIECE_IS_ACTIVE( plies->piece ) ? plies->piece
                                                  : CC_PTE_None;

    // <!> Shadows issue if ply is not contained in plies.
    //
    // if ( ply__d && CC_PIECE_IS_ACTIVE( ply__d->piece ) )
    //     return CC_PIECE_IS_VALID( ply__d->piece ) ? ply__d->piece
    //                                               : CC_PTE_None;

    CcPieceTagType activator = CC_PTE_None;
    bool ply_encountered = false;
    CcPly * p = plies;

    while ( p ) {
        if ( !CC_PIECE_IS_VALID( p->piece ) )
            return CC_PTE_None;

        if ( CC_PIECE_IS_ACTIVE( p->piece ) )
            activator = p->piece;

        if ( p == ply__d ) {
            ply_encountered = true;
            break;
        }

        p = p->next;
    }

    if ( ply__d && !ply_encountered )
        return CC_PTE_None;
    else
        return activator;
}

char * cc_ply_all_to_string__new( CcPly * plies ) {
    if ( !plies ) return NULL;

    //
    // Count plies, steps.

    CcPly * p_count = plies;
    size_t count_plies = 0;
    size_t count_steps = 0;

    while ( p_count ) {
        ++count_plies;
        count_steps += cc_step_count( p_count->steps );

        p_count = p_count->next;
    }

    //
    // Calc max string size, allocate.

    size_t step_len = CC_MAX_LEN_CHAR_8 + CC_MAX_LEN_CHAR_16 + 2;
                      // CC_MAX_LEN_CHAR_8, for position
                      // + CC_MAX_LEN_CHAR_16, for side-effect
                      // + 2, for step links, e.g. ".." before step

    size_t plies_size = ( count_plies * CC_MAX_LEN_PLY_LINK_TYPE_SYMBOL )
                      + ( count_steps * step_len )
                      + 1; // +1, for '\0'

    char * plies_str__a = calloc( plies_size, sizeof( char ) );
    if ( !plies_str__a ) return NULL;

    char const * plies_end__w = plies_str__a + plies_size;

    //
    // Collect ply string, append to result.

    CcPly * p = plies;
    char * s = plies_str__a;

    while ( p ) {
        // Append ply link symbol.

        char const * pl = cc_ply_link_type_symbol( p->link );
        char const * ple_end__w = cc_str_append_into( s, plies_end__w, CC_SIZE_IGNORE, pl, NULL, CC_MAX_LEN_PLY_LINK_TYPE_SYMBOL );
        if ( !ple_end__w ) {
            CC_FREE( plies_str__a );
            return NULL;
        }
        s = (char *)ple_end__w;

        // Append piece symbol, lost tag.

        char piece_symbol = cc_piece_symbol( p->piece );
        *s++ = piece_symbol;

        char const * lte_str = cc_losing_tag_symbol( p->lost_tag );
        char const * lte_end__w = cc_str_append_into( s, plies_end__w, CC_SIZE_IGNORE, lte_str, NULL, CC_MAX_LEN_LOSING_TAG_SYMBOL );
        if ( !lte_end__w ) {
            CC_FREE( plies_str__a );
            return NULL;
        }
        s = (char *)lte_end__w;

        // Append steps.

        char * steps_str__a = cc_step_all_to_string__new( p->steps );
        if ( !steps_str__a ) {
            CC_FREE( plies_str__a );
            return NULL;
        }

        char const * steps_end__w = cc_str_append_into( s, plies_end__w, CC_SIZE_IGNORE, steps_str__a, NULL, CC_MAX_LEN_BUFFER );
        if ( !steps_end__w ) {
            CC_FREE( steps_str__a );
            CC_FREE( plies_str__a );
            return NULL;
        }
        s = (char *)steps_end__w;

        CC_FREE( steps_str__a );
        p = p->next;
    }

    return plies_str__a;
}
