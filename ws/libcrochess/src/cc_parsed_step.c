// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_parsed_step.h"


char const * cc_parsed_step_link_symbol( CcParsedStepLinkEnum sle ) {
    switch ( sle ) {
        case CC_PSLE_None : return NULL;
        case CC_PSLE_Start : return "";
        case CC_PSLE_Reposition : return ",";
        case CC_PSLE_Next : return ".";
        case CC_PSLE_Distant : return "..";
        case CC_PSLE_Destination : return "-";
        case CC_PSLE_JustDestination : return "";

        default : return NULL;
    }
}

CcParsedStep * cc_parsed_step__new( CcParsedStepLinkEnum link,
                                    CcPos field,
                                    CcParsedSideEffect side_effect ) {
    CcParsedStep * step__a = malloc( sizeof( CcParsedStep ) );
    if ( !step__a ) return NULL;

    step__a->link = link;
    step__a->field = field;
    step__a->side_effect = side_effect;

    step__a->next = NULL;

    return step__a;
}

CcParsedStep * cc_parsed_step_append( CcParsedStep ** steps__iod_a,
                                      CcParsedStepLinkEnum link,
                                      CcPos field,
                                      CcParsedSideEffect side_effect ) {
    if ( !steps__iod_a ) return NULL;

    CcParsedStep * step__t = cc_parsed_step__new( link, field, side_effect );
    if ( !step__t ) return NULL;

    if ( !*steps__iod_a ) {
        *steps__iod_a = step__t; // Ownership transfer.
    } else {
        CcParsedStep * s = *steps__iod_a;
        CC_FASTFORWARD( s );
        s->next = step__t; // Append + ownership transfer.
    }

    return step__t; // Weak pointer.
}

CcParsedStep * cc_parsed_step_duplicate_all__new( CcParsedStep * steps ) {
    if ( !steps ) return NULL;

    CcParsedStep * steps__a = NULL;
    CcParsedStep * from = steps;

    while ( from ) {
        CcParsedStep * step__w = cc_parsed_step_append( &steps__a,
                                           from->link,
                                           from->field,
                                           from->side_effect );
        if ( !step__w ) { // Failed append --> ownership not transferred ...
            cc_parsed_step_free_all( &steps__a );
            return NULL;
        }

        from = from->next;
    }

    return steps__a;
}

CcParsedStep * cc_parsed_step_extend( CcParsedStep ** steps__iod_a,
                                      CcParsedStep ** steps__d_n ) {
    if ( !steps__iod_a ) return NULL;
    if ( !steps__d_n ) return NULL;

    if ( !*steps__d_n ) return *steps__iod_a;

    if ( !*steps__iod_a ) {
        // Ownership transfer.
        *steps__iod_a = *steps__d_n;
        *steps__d_n = NULL;

        return *steps__iod_a;
    }

    CcParsedStep * last = *steps__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *steps__d_n;
    *steps__d_n = NULL;

    return last->next;
}

size_t cc_parsed_step_count( CcParsedStep * steps ) {
    if ( !steps ) return 0;

    size_t count = 0;
    CcParsedStep * s = steps;

    while ( s ) {
        ++count;
        s = s->next;
    }

    return count;
}

CcParsedStep * cc_parsed_step_find_start( CcParsedStep * steps ) {
    if ( !steps ) return NULL;

    if ( steps->link == CC_PSLE_Start ) {
        CcParsedStep * s = steps->next;

        while ( s ) {
            if ( s->link == CC_PSLE_Start )
                return NULL;

            s = s->next;
        }

        return steps;
    }

    return NULL;
}

CcParsedStep * cc_parsed_step_find_destination( CcParsedStep * steps ) {
    if ( !steps ) return NULL;

    CcParsedStep * prev = NULL;
    CcParsedStep * s = steps;
    bool prev_step = false;

    while ( s ) {
        if ( s->link == CC_PSLE_None ) return NULL;

        if ( s->next ) {
            if ( CC_PARSED_STEP_LINK_IS_DESTINATION( s->link ) ) return NULL; // An intermediate destination?

            if ( s->link == CC_PSLE_Reposition ) {
                // Reposition is legal only on 1st, or 2nd step.
                if ( ( s != steps ) && ( prev != steps ) ) return NULL;
            }

            if ( s->link == CC_PSLE_Start ) {
                // Start is legal only on 1st step.
                if ( s != steps ) return NULL;
            }

            prev_step = true;

            prev = s;
            s = s->next;
        } else {
            if ( prev_step && ( s->link == CC_PSLE_Start ) )
                return NULL;
            else
                return s;
        }
    }

    return NULL; // Has to be here, clang complains otherwise.
}

bool cc_parsed_step_free_all( CcParsedStep ** steps__f ) {
    if ( !steps__f ) return false;
    if ( !*steps__f ) return true;

    CcParsedStep * s = *steps__f;

    while ( s ) {
        CcParsedStep * tmp = s->next;
        CC_FREE( s );
        s = tmp;
    }

    *steps__f = NULL;
    return true;
}

char * cc_parsed_step_all_to_short_string__new( CcParsedStep * steps ) {
    if ( !steps ) return NULL;

    // unused len is certainly > 0, because steps != NULL
    signed int unused = cc_parsed_step_count( steps ) *
                        ( CC_MAX_LEN_CHAR_8 + CC_MAX_LEN_CHAR_16 + 2 ) + 1; // +1, for '\0'
                        // CC_MAX_LEN_CHAR_8, for position
                        // + CC_MAX_LEN_CHAR_16, for side-effect
                        // + 2, for step links, e.g. ".." before step

    char * steps_str__a = malloc( unused );
    if ( !steps_str__a ) return NULL;

    // Must be zero-terminated.
    if ( !cc_str_clear( steps_str__a, unused ) ) {
        CC_FREE( steps_str__a );
        return NULL;
    }

    char * steps_str = steps_str__a;
    char * steps_end = steps_str;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    cc_char_16 se_c16 = CC_CHAR_16_EMPTY;
    CcParsedStep * s = steps;

    while ( s && ( unused > 1 ) ) { // 1, reserved for '\0'
        char const * sle_str = cc_parsed_step_link_symbol( s->link );

        if ( sle_str ) {
            char * sle_end = cc_str_append_into( steps_str, unused, sle_str, CC_MAX_LEN_PARSED_STEP_LINK_SYMBOL );
            if ( !sle_end ) {
                CC_FREE( steps_str__a );
                return NULL;
            }

            unused -= ( sle_end - steps_str );
            steps_str = sle_end;
        }

        if ( !cc_pos_to_short_string( s->field, &pos_c8 ) ) {
            CC_FREE( steps_str__a );
            return NULL;
        }

        steps_end = cc_str_append_into( steps_str, unused, pos_c8, CC_MAX_LEN_CHAR_8 );
        if ( !steps_end ) {
            CC_FREE( steps_str__a );
            return NULL;
        }

        unused -= ( steps_end - steps_str );
        steps_str = steps_end;

        if ( !cc_parsed_side_effect_to_short_str( s->side_effect, &se_c16 ) ) {
            CC_FREE( steps_str__a );
            return NULL;
        }

        steps_end = cc_str_append_into( steps_str, unused, se_c16, CC_MAX_LEN_CHAR_16 );
        if ( !steps_end ) {
            CC_FREE( steps_str__a );
            return NULL;
        }

        unused -= ( steps_end - steps_str );
        steps_str = steps_end;

        s = s->next;
    }

    return steps_str__a;
}


//
// new conveniences

CcParsedStep * cc_parsed_step_none__new( CcParsedStepLinkEnum link, CcPos field ) {
    CcParsedSideEffect se = cc_parsed_side_effect_none();
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_capture__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceType piece,
                                            CcLosingTagEnum lost_tag ) {
    CcParsedSideEffect se = cc_parsed_side_effect_capture( piece, lost_tag );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_displacement__new( CcParsedStepLinkEnum link, CcPos field,
                                                 CcPieceType piece,
                                                 CcLosingTagEnum lost_tag,
                                                 CcPos destination ) {
    CcParsedSideEffect se = cc_parsed_side_effect_displacement( piece, lost_tag, destination );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_en_passant__new( CcParsedStepLinkEnum link, CcPos field,
                                               CcPieceType pawn,
                                               CcPos distant ) {
    CcParsedSideEffect se = cc_parsed_side_effect_en_passant( pawn, distant );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_castle__new( CcParsedStepLinkEnum link, CcPos field,
                                           CcPieceType rook,
                                           CcPos start,
                                           CcPos destination ) {
    CcParsedSideEffect se = cc_parsed_side_effect_castle( rook, start, destination );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_promote__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceType captured,
                                            CcLosingTagEnum lost_tag,
                                            CcPieceType promoted_to ) {
    CcParsedSideEffect se = cc_parsed_side_effect_promote( captured, lost_tag, promoted_to );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_tag_for_promotion__new( CcParsedStepLinkEnum link, CcPos field,
                                                      CcPieceType captured,
                                                      CcLosingTagEnum lost_tag ) {
    CcParsedSideEffect se = cc_parsed_side_effect_tag_for_promotion( captured, lost_tag );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_convert__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceType piece,
                                            CcLosingTagEnum lost_tag ) {
    CcParsedSideEffect se = cc_parsed_side_effect_convert( piece, lost_tag );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_failed_conversion__new( CcParsedStepLinkEnum link, CcPos field ) {
    CcParsedSideEffect se = cc_parsed_side_effect_failed_conversion();
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_demote__new( CcParsedStepLinkEnum link, CcPos field,
                                           CcPieceType piece,
                                           CcLosingTagEnum lost_tag,
                                           CcPos distant ) {
    CcParsedSideEffect se = cc_parsed_side_effect_demote( piece, lost_tag, distant );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_resurrect__new( CcParsedStepLinkEnum link, CcPos field,
                                              CcPieceType piece,
                                              CcPos destination ) {
    CcParsedSideEffect se = cc_parsed_side_effect_resurrect( piece, destination );
    return cc_parsed_step__new( link, field, se );
}

CcParsedStep * cc_parsed_step_failed_resurrection__new( CcParsedStepLinkEnum link, CcPos field ) {
    CcParsedSideEffect se = cc_parsed_side_effect_failed_resurrection();
    return cc_parsed_step__new( link, field, se );
}

//
// append conveniences

CcParsedStep * cc_parsed_step_none_append( CcParsedStep ** steps__iod_a,
                                           CcParsedStepLinkEnum link,
                                           CcPos field ) {
    CcParsedSideEffect se = cc_parsed_side_effect_none();
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_capture_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceType piece,
                                              CcLosingTagEnum lost_tag ) {
    CcParsedSideEffect se = cc_parsed_side_effect_capture( piece, lost_tag );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_displacement_append( CcParsedStep ** steps__iod_a,
                                                   CcParsedStepLinkEnum link,
                                                   CcPos field,
                                                   CcPieceType piece,
                                                   CcLosingTagEnum lost_tag,
                                                   CcPos destination ) {
    CcParsedSideEffect se = cc_parsed_side_effect_displacement( piece, lost_tag, destination );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_en_passant_append( CcParsedStep ** steps__iod_a,
                                                 CcParsedStepLinkEnum link,
                                                 CcPos field,
                                                 CcPieceType pawn,
                                                 CcPos distant ) {
    CcParsedSideEffect se = cc_parsed_side_effect_en_passant( pawn, distant );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_castle_append( CcParsedStep ** steps__iod_a,
                                             CcParsedStepLinkEnum link,
                                             CcPos field,
                                             CcPieceType rook,
                                             CcPos start,
                                             CcPos destination ) {
    CcParsedSideEffect se = cc_parsed_side_effect_castle( rook, start, destination );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_promote_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceType captured,
                                              CcLosingTagEnum lost_tag,
                                              CcPieceType promoted_to ) {
    CcParsedSideEffect se = cc_parsed_side_effect_promote( captured, lost_tag, promoted_to );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_tag_for_promotion_append( CcParsedStep ** steps__iod_a,
                                                        CcParsedStepLinkEnum link,
                                                        CcPos field,
                                                        CcPieceType captured,
                                                        CcLosingTagEnum lost_tag ) {
    CcParsedSideEffect se = cc_parsed_side_effect_tag_for_promotion( captured, lost_tag );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_convert_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceType piece,
                                              CcLosingTagEnum lost_tag ) {
    CcParsedSideEffect se = cc_parsed_side_effect_convert( piece, lost_tag );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_failed_conversion_append( CcParsedStep ** steps__iod_a,
                                                        CcParsedStepLinkEnum link,
                                                        CcPos field ) {
    CcParsedSideEffect se = cc_parsed_side_effect_failed_conversion();
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_demote_append( CcParsedStep ** steps__iod_a,
                                             CcParsedStepLinkEnum link,
                                             CcPos field,
                                             CcPieceType piece,
                                             CcLosingTagEnum lost_tag,
                                             CcPos distant ) {
    CcParsedSideEffect se = cc_parsed_side_effect_demote( piece, lost_tag, distant );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_resurrect_append( CcParsedStep ** steps__iod_a,
                                                CcParsedStepLinkEnum link,
                                                CcPos field,
                                                CcPieceType piece,
                                                CcPos destination ) {
    CcParsedSideEffect se = cc_parsed_side_effect_resurrect( piece, destination );
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}

CcParsedStep * cc_parsed_step_failed_resurrection_append( CcParsedStep ** steps__iod_a,
                                                          CcParsedStepLinkEnum link,
                                                          CcPos field ) {
    CcParsedSideEffect se = cc_parsed_side_effect_failed_resurrection();
    return cc_parsed_step_append( steps__iod_a, link, field, se );
}
