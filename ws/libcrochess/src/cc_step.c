// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"


char const * cc_step_link_type_symbol( CcStepLinkTypeEnum slte ) {
    switch ( slte ) {
        case CC_SLTE_None : return NULL;
        case CC_SLTE_InitialPosition : return "";
        case CC_SLTE_Reposition : return "\\";
        case CC_SLTE_Next : return ".";
        case CC_SLTE_Distant : return "..";
        case CC_SLTE_Destination : return "-";
        case CC_SLTE_JustDestination : return "";

        default : return NULL;
    }
}

CcMaybeBoolEnum cc_step_link_type_is_congruent( CcStepLinkTypeEnum slte_1,
                                                CcStepLinkTypeEnum slte_2 ) {
    if ( !CC_STEP_LINK_TYPE_IS_ENUMERATOR( slte_2 ) ) return CC_MBE_Void;
    if ( slte_2 == CC_SLTE_None ) return CC_MBE_False;

    switch ( slte_1 ) {
        case CC_SLTE_None : return CC_MBE_False;

        case CC_SLTE_InitialPosition : return CC_BOOL_TO_MAYBE( slte_2 == CC_SLTE_InitialPosition );

        case CC_SLTE_Reposition : return CC_BOOL_TO_MAYBE( slte_2 == CC_SLTE_Reposition );

        case CC_SLTE_Next :
        case CC_SLTE_Distant :
        case CC_SLTE_Destination :
        case CC_SLTE_JustDestination : {
            return CC_BOOL_TO_MAYBE( slte_2 == CC_SLTE_Next || \
                                     slte_2 == CC_SLTE_Distant || \
                                     slte_2 == CC_SLTE_Destination || \
                                     slte_2 == CC_SLTE_JustDestination );
        }

        default : return CC_MBE_Void;
    }
}


CcStep * cc_step__new( CcStepLinkTypeEnum link,
                       CcPos field,
                       CcSideEffect side_effect ) {
    CcStep * step__a = CC_MALLOC( sizeof( CcStep ) );
    if ( !step__a ) return NULL;

    step__a->link = link;
    step__a->field = field;
    step__a->side_effect = side_effect;

    step__a->tentative__d = NULL;

    step__a->next = NULL;

    return step__a;
}

CcStep * cc_step_initial_no_side_effect__new( CcPos field ) {
    return cc_step__new( CC_SLTE_InitialPosition, field, cc_side_effect_none() );
}

CcStep * cc_step_initial__new( CcPos field,
                               CcSideEffect side_effect ) {
    return cc_step__new( CC_SLTE_InitialPosition, field, side_effect );
}

CcStep * cc_step_next_no_side_effect__new( CcPos field ) {
    return cc_step__new( CC_SLTE_Next, field, cc_side_effect_none() );
}

CcStep * cc_step_next__new( CcPos field,
                            CcSideEffect side_effect ) {
    return cc_step__new( CC_SLTE_Next, field, side_effect );
}

CcStep * cc_step_append( CcStep ** steps__iod_a,
                         CcStepLinkTypeEnum link,
                         CcPos field,
                         CcSideEffect side_effect ) {
    if ( !steps__iod_a ) return NULL;

    CcStep * step__t = cc_step__new( link, field, side_effect );
    if ( !step__t ) return NULL;

    if ( !*steps__iod_a ) {
        *steps__iod_a = step__t; // Ownership transfer.
    } else {
        CcStep * s = *steps__iod_a;
        CC_FASTFORWARD( s );
        s->next = step__t; // Append + ownership transfer.
    }

    return step__t; // Weak pointer.
}

CcStep * cc_step_append_next_no_side_effect( CcStep ** steps__iod_a,
                                             CcPos field ) {
    return cc_step_append( steps__iod_a, CC_SLTE_Next, field, cc_side_effect_none() );
}

CcStep * cc_step_duplicate_all__new( CcStep * steps,
                                     bool include_tentative ) {
    if ( !steps ) return NULL;

    CcStep * steps__a = NULL;
    CcStep * from = steps;

    while ( from ) {
        CcStep * step__w = cc_step_append( &steps__a,
                                           from->link,
                                           from->field,
                                           from->side_effect );
        if ( !step__w ) { // Failed append --> ownership not transferred ...
            cc_step_free_all( &steps__a );
            return NULL;
        }

        if ( include_tentative && from->tentative__d ) {
            CcSideEffectLink * sel__t = cc_side_effect_link_duplicate_all__new( from->tentative__d );
            if ( !sel__t ) {
                cc_step_free_all( &steps__a );
                return NULL;
            }

            if ( !cc_side_effect_link_extend( &( step__w->tentative__d ), &sel__t ) ) {
                cc_side_effect_link_free_all( &sel__t ); // If cc_side_effect_link_extend() failed, ownership was not transferred.
                cc_step_free_all( &steps__a );
                return NULL;
            }
        }

        from = from->next;
    }

    return steps__a;
}

CcStep * cc_step_extend( CcStep ** steps__iod_a,
                         CcStep ** steps__d_n ) {
    if ( !steps__iod_a ) return NULL;
    if ( !steps__d_n ) return NULL;

    if ( !*steps__d_n ) return *steps__iod_a;

    if ( !*steps__iod_a ) {
        // Ownership transfer.
        *steps__iod_a = *steps__d_n;
        *steps__d_n = NULL;

        return *steps__iod_a;
    }

    CcStep * last = *steps__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *steps__d_n;
    *steps__d_n = NULL;

    return last->next;
}

size_t cc_step_count( CcStep * steps, bool do_momentum ) {
    if ( !steps ) return 0;

    size_t count = 0;
    CcStep * s = steps;

    if ( do_momentum ) {
        if ( s->link == CC_SLTE_InitialPosition ) {
            s = s->next;

            if ( !s ) return 0;

            if ( s->link == CC_SLTE_Reposition ) {
                s = s->next;
            } else if ( s->link != CC_SLTE_Next )
                return 0;
        } else
            return 0;

        while ( s ) {
            if ( s->link == CC_SLTE_Next ) {
                ++count;
                s = s->next;
            } else
                return 0;
        }
    } else {
        while ( s ) {
            ++count;
            s = s->next;
        }
    }

    return count;
}

CcStep * cc_step_fetch_initial( CcStep * steps ) {
    if ( !steps ) return NULL;

    if ( steps->link == CC_SLTE_InitialPosition ) {
        CcStep * s = steps->next;

        while ( s ) {
            if ( s->link == CC_SLTE_InitialPosition )
                return NULL;

            s = s->next;
        }

        return steps;
    }

    return NULL;
}

CcStep * cc_step_fetch_destination( CcStep * steps ) {
    if ( !steps ) return NULL;

    CcStep * prev = NULL;
    CcStep * s = steps;
    bool prev_step = false;

    while ( s ) {
        if ( s->link == CC_SLTE_None ) return NULL;

        if ( s->next ) {
            if ( CC_STEP_LINK_TYPE_IS_DESTINATION( s->link ) ) return NULL; // An intermediate destination?

            if ( s->link == CC_SLTE_Reposition ) {
                // Reposition is legal only on 1st, or 2nd step.
                if ( ( s != steps ) && ( prev != steps ) ) return NULL;
            }

            if ( s->link == CC_SLTE_InitialPosition ) {
                // Start is legal only on 1st step.
                if ( s != steps ) return NULL;
            }

            prev_step = true;

            prev = s;
            s = s->next;
        } else {
            if ( prev_step && ( s->link == CC_SLTE_InitialPosition ) )
                return NULL;
            else
                return s; // Destination is any last step, regardless of separator.
        }
    }

    return NULL; // Has to be here, clang complains otherwise.
}

CcSideEffect * cc_step_fetch_last_side_effect( CcStep * steps ) {
    if ( !steps ) return NULL;

    CcStep * last = cc_step_fetch_destination( steps );
    if ( !last ) return NULL;

    return &( last->side_effect );
}

CcMaybeBoolEnum cc_step_is_congruent( CcStep * step_1, CcStep * step_2 ) {
    if ( !step_1 ) return CC_MBE_Void;
    if ( !step_2 ) return CC_MBE_Void;

    if ( step_1->link != step_2->link ) return CC_MBE_False;



    return CC_MBE_False; // TODO :: FIX
}

bool cc_step_free_all( CcStep ** steps__f ) {
    if ( !steps__f ) return false;
    if ( !*steps__f ) return true;

    CcStep * s = *steps__f;
    bool result = true;

    while ( s ) {
        CcStep * tmp = s->next;

        if ( s->tentative__d )
            result = cc_side_effect_link_free_all( &( s->tentative__d ) ) && result;

        CC_FREE( s );
        s = tmp;
    }

    *steps__f = NULL;
    return result;
}

static size_t _cc_step_sum_len_all_tentative( CcStep * steps ) {
    if ( !steps ) return 0;

    size_t len = 0;
    CcStep * s = steps;

    while ( s ) {
        if ( s->tentative__d ) {
            len += cc_side_effect_link_len( s->tentative__d );
        }

        s = s->next;
    }

    return len;
}

char * cc_step_all_to_string__new( CcStep * steps ) {
    if ( !steps ) return NULL;

    size_t se_len = _cc_step_sum_len_all_tentative( steps ); // length of all tentative__d side-effects in a complete linked list

    // unused len is certainly > 0, because steps != NULL
    size_t steps_len = cc_step_count( steps, false ) *
                       ( CC_MAX_LEN_CHAR_8 + CC_MAX_LEN_CHAR_16 + 2 ) +
                       // CC_MAX_LEN_CHAR_8, for position
                       // + CC_MAX_LEN_CHAR_16, for side-effect
                       // + 2, for step links, e.g. ".." before step
                       ( se_len * ( CC_MAX_LEN_CHAR_16 + 1 ) + 2 );
                       // CC_MAX_LEN_CHAR_16, for each side-effect
                       // + 1, for ',' between every two side-effects
                       // + 2, for '{' and '}' enclosing tentative__d side-effects list

    size_t steps_size = steps_len + 1; // +1, for '\0'

    char * steps_str__a = calloc( steps_size, sizeof( char ) );
    if ( !steps_str__a ) return NULL;

    char const * steps_end__w = steps_str__a + steps_size;

    char * steps_str = steps_str__a;
    // char * steps_end = steps_str;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    cc_char_16 se_c16 = CC_CHAR_16_EMPTY;
    CcStep * s = steps;

    while ( s ) { // 1, reserved for '\0'
        char const * sle_str = cc_step_link_type_symbol( s->link );

        if ( sle_str ) {
            char const * sle_end__w = cc_str_append_into( steps_str, steps_end__w, CC_SIZE_IGNORE, sle_str, NULL, CC_MAX_LEN_STEP_LINK_TYPE_SYMBOL );
            if ( !sle_end__w ) {
                CC_FREE( steps_str__a );
                return NULL;
            }
            steps_str = (char *)sle_end__w;
        }

        if ( !cc_pos_to_string( s->field, &pos_c8 ) ) {
            CC_FREE( steps_str__a );
            return NULL;
        }

        char const * pos_end__w = cc_str_append_into( steps_str, steps_end__w, CC_SIZE_IGNORE, pos_c8, NULL, CC_MAX_LEN_CHAR_8 );
        if ( !pos_end__w ) {
            CC_FREE( steps_str__a );
            return NULL;
        }
        steps_str = (char *)pos_end__w;

        if ( !cc_side_effect_to_str( s->side_effect, &se_c16 ) ) {
            CC_FREE( steps_str__a );
            return NULL;
        }

        char const * se_end__w = cc_str_append_into( steps_str, steps_end__w, CC_SIZE_IGNORE, se_c16, NULL, CC_MAX_LEN_CHAR_16 );
        if ( !se_end__w ) {
            CC_FREE( steps_str__a );
            return NULL;
        }
        steps_str = (char *)se_end__w;

        if ( s->tentative__d ) {
            char * sel_end__a = cc_side_effect_link_to_string__new( s->tentative__d );
            if ( !sel_end__a ) {
                CC_FREE( steps_str__a );
                return NULL;
            }

            *steps_str++ = '{';

            char const * se_list__w = cc_str_append_into( steps_str, steps_end__w, CC_SIZE_IGNORE, sel_end__a, NULL, CC_MAX_LEN_ZERO_TERMINATED );
            if ( !se_list__w ) {
                CC_FREE( sel_end__a );
                CC_FREE( steps_str__a );
                return NULL;
            }
            steps_str = (char *)sel_end__a;

            *steps_str++ = '}';

            CC_FREE( sel_end__a );
        }

        s = s->next;
    }

    return steps_str__a;
}


//
// new conveniences

CcStep * cc_step_none__new( CcStepLinkTypeEnum link, CcPos field ) {
    CcSideEffect se = cc_side_effect_none();
    return cc_step__new( link, field, se );
}

CcStep * cc_step_capture__new( CcStepLinkTypeEnum link, CcPos field,
                               CcPieceTagType piece ) {
    CcSideEffect se = cc_side_effect_capture( piece );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_displacement__new( CcStepLinkTypeEnum link, CcPos field,
                                    CcPieceTagType piece,
                                    CcPos destination ) {
    CcSideEffect se = cc_side_effect_displacement( piece, destination );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_en_passant__new( CcStepLinkTypeEnum link, CcPos field,
                                  CcPieceTagType private,
                                  CcPos distant ) {
    CcSideEffect se = cc_side_effect_en_passant( private, distant );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_castle__new( CcStepLinkTypeEnum link, CcPos field,
                              CcPieceTagType rook,
                              CcPos start,
                              CcPos destination ) {
    CcSideEffect se = cc_side_effect_castle( rook, start, destination );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_promote__new( CcStepLinkTypeEnum link, CcPos field,
                               CcPieceTagType captured,
                               CcPieceTagType promoted_to ) {
    CcSideEffect se = cc_side_effect_promote( captured, promoted_to );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_tag_for_promotion__new( CcStepLinkTypeEnum link, CcPos field,
                                         CcPieceTagType captured ) {
    CcSideEffect se = cc_side_effect_tag_for_promotion( captured );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_convert__new( CcStepLinkTypeEnum link, CcPos field,
                               CcPieceTagType piece ) {
    CcSideEffect se = cc_side_effect_convert( piece );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_failed_conversion__new( CcStepLinkTypeEnum link, CcPos field ) {
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step__new( link, field, se );
}

CcStep * cc_step_demote__new( CcStepLinkTypeEnum link, CcPos field,
                              CcPieceTagType piece,
                              CcPos distant ) {
    CcSideEffect se = cc_side_effect_demote( piece, distant );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_resurrect__new( CcStepLinkTypeEnum link, CcPos field,
                                 CcPieceTagType piece,
                                 CcPos destination ) {
    CcSideEffect se = cc_side_effect_resurrect( piece, destination );
    return cc_step__new( link, field, se );
}

CcStep * cc_step_failed_resurrection__new( CcStepLinkTypeEnum link, CcPos field ) {
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step__new( link, field, se );
}

//
// append conveniences

CcStep * cc_step_none_append( CcStep ** steps__iod_a,
                              CcStepLinkTypeEnum link,
                              CcPos field ) {
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_capture_append( CcStep ** steps__iod_a,
                                 CcStepLinkTypeEnum link,
                                 CcPos field,
                                 CcPieceTagType piece ) {
    CcSideEffect se = cc_side_effect_capture( piece );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_displacement_append( CcStep ** steps__iod_a,
                                      CcStepLinkTypeEnum link,
                                      CcPos field,
                                      CcPieceTagType piece,
                                      CcPos destination ) {
    CcSideEffect se = cc_side_effect_displacement( piece, destination );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_en_passant_append( CcStep ** steps__iod_a,
                                    CcStepLinkTypeEnum link,
                                    CcPos field,
                                    CcPieceTagType private,
                                    CcPos distant ) {
    CcSideEffect se = cc_side_effect_en_passant( private, distant );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_castle_append( CcStep ** steps__iod_a,
                                CcStepLinkTypeEnum link,
                                CcPos field,
                                CcPieceTagType rook,
                                CcPos start,
                                CcPos destination ) {
    CcSideEffect se = cc_side_effect_castle( rook, start, destination );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_promote_append( CcStep ** steps__iod_a,
                                 CcStepLinkTypeEnum link,
                                 CcPos field,
                                 CcPieceTagType captured,
                                 CcPieceTagType promoted_to ) {
    CcSideEffect se = cc_side_effect_promote( captured, promoted_to );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_tag_for_promotion_append( CcStep ** steps__iod_a,
                                           CcStepLinkTypeEnum link,
                                           CcPos field,
                                           CcPieceTagType captured ) {
    CcSideEffect se = cc_side_effect_tag_for_promotion( captured );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_convert_append( CcStep ** steps__iod_a,
                                 CcStepLinkTypeEnum link,
                                 CcPos field,
                                 CcPieceTagType piece ) {
    CcSideEffect se = cc_side_effect_convert( piece );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_failed_conversion_append( CcStep ** steps__iod_a,
                                           CcStepLinkTypeEnum link,
                                           CcPos field ) {
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_demote_append( CcStep ** steps__iod_a,
                                CcStepLinkTypeEnum link,
                                CcPos field,
                                CcPieceTagType piece,
                                CcPos distant ) {
    CcSideEffect se = cc_side_effect_demote( piece, distant );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_resurrect_append( CcStep ** steps__iod_a,
                                   CcStepLinkTypeEnum link,
                                   CcPos field,
                                   CcPieceTagType piece,
                                   CcPos destination ) {
    CcSideEffect se = cc_side_effect_resurrect( piece, destination );
    return cc_step_append( steps__iod_a, link, field, se );
}

CcStep * cc_step_failed_resurrection_append( CcStep ** steps__iod_a,
                                             CcStepLinkTypeEnum link,
                                             CcPos field ) {
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append( steps__iod_a, link, field, se );
}
