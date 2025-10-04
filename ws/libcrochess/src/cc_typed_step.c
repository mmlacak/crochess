// Copyright (c) 2021, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_math.h"

#include "cc_typed_step.h"


//
// Typed step

CcTypedStep cc_typed_step( CcPos step, CcStepTypeEnum type ) {
    CcTypedStep st = { .step = step, .type = type };
    return st;
}

bool cc_typed_step_is_equal( CcTypedStep ts_1, CcTypedStep ts_2 ) {
    return CC_TYPED_STEP_IS_EQUAL( ts_1, ts_2 );
}


//
// Linked typed steps.

CcTypedStepLink * cc_typed_step_link__new( CcTypedStep step ) {
    CcTypedStepLink * pl__a = CC_MALLOC( sizeof( CcTypedStepLink ) );
    if ( !pl__a ) return NULL;

    pl__a->step = step;
    pl__a->next = NULL;

    return pl__a;
}

CcTypedStepLink * cc_typed_step_link_append( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStep step ) {
    if ( !ts_link__iod_a ) return NULL;

    CcTypedStepLink * tsl__t = cc_typed_step_link__new( step );
    if ( !tsl__t ) return NULL;

    if ( !*ts_link__iod_a ) {
        *ts_link__iod_a = tsl__t; // Ownership transfer.
    } else {
        CcTypedStepLink * tsl = *ts_link__iod_a;
        CC_FASTFORWARD( tsl );
        tsl->next = tsl__t; // Append + ownership transfer.
    }

    return tsl__t; // Weak pointer.
}

CcTypedStepLink * cc_typed_step_link_extend( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStepLink ** ts_link__n ) {
    if ( !ts_link__iod_a ) return NULL;
    if ( !ts_link__n ) return NULL;

    if ( !*ts_link__n ) return *ts_link__iod_a;

    if ( !*ts_link__iod_a ) {
        // Ownership transfer.
        *ts_link__iod_a = *ts_link__n;
        *ts_link__n = NULL;

        return *ts_link__iod_a;
    }

    CcTypedStepLink * last = *ts_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *ts_link__n;
    *ts_link__n = NULL;

    return last->next;
}

bool cc_typed_step_link_free_all( CcTypedStepLink ** ts_link__f ) {
    if ( !ts_link__f ) return false;
    if ( !*ts_link__f ) return true;

    CcTypedStepLink * tsl = *ts_link__f;
    CcTypedStepLink * tmp = NULL;

    while ( tsl ) {
        tmp = tsl->next;
        CC_FREE( tsl );
        tsl = tmp;
    }

    *ts_link__f = NULL;
    return true;
}

bool cc_typed_step_link_are_all_valid( CcTypedStepLink * ts_link ) {
    if ( !ts_link ) return false;

    CcTypedStepLink * tsl = ts_link;

    while ( tsl ) {
        if ( !CC_TYPED_STEP_IS_VALID( tsl->step ) ) return false;
        tsl = tsl->next;
    }

    return true;
}

size_t cc_typed_step_link_len( CcTypedStepLink * ts_link ) {
    if ( !ts_link ) return 0;

    size_t len = 0;
    CcTypedStepLink * tsl = ts_link;

    while ( tsl ) {
        ++len;
        tsl = tsl->next;
    }

    return len;
}

char * cc_typed_step_link_to_string__new( CcTypedStepLink * ts_link ) {
    if ( !ts_link ) return NULL;

    // unused len is certainly > 0, because ts_link != NULL
    size_t ts_len = cc_typed_step_link_len( ts_link ) *
                    ( CC_MAX_LEN_CHAR_8 + 1 );
                    // CC_MAX_LEN_CHAR_8, for position + piece
                    // +1, for separator '.' between positions

    size_t ts_size = ts_len + 1; // +1, for '\0'

    char * pl_str__a = calloc( ts_size, sizeof( char ) );
    if ( !pl_str__a ) return NULL;

    *pl_str__a = '\0';

    char const * pl_end__w = pl_str__a + ts_size;

    char * pl_str = pl_str__a;
    // char * pl_end = pl_str;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    CcTypedStepLink * tsl = ts_link;

    while ( tsl ) {
        if ( tsl != ts_link ) { // Not 1st pos ...
            *pl_str++ = '.';
            *pl_str = '\0';
        }

        if ( !cc_pos_to_string( tsl->step.step, &pos_c8 ) ) {
            CC_FREE( pl_str__a );
            return NULL;
        }

        char const * pos_end__w = cc_str_append_into( pl_str, pl_end__w, CC_SIZE_IGNORE, pos_c8, NULL, CC_MAX_LEN_CHAR_8 );
        if ( !pos_end__w ) {
            CC_FREE( pl_str__a );
            return NULL;
        }
        pl_str = (char *)pos_end__w;

        tsl = tsl->next;
    }

    return pl_str__a;
}

// todo :: DELETE
// //
// // Typed step definition

// bool cc_typed_step_def( CcTypedStep step__d,
//                         CcTypedStep step_2__d,
//                         CcTypedStepLink ** steps__d_n,
//                         CcTypedStepDef * step_def__o ) {
//     if ( !step_def__o ) return false;

//     if ( CC_TYPED_STEP_IS_VALID( step__d ) ) {
//         if ( steps__d_n ) return false;

//         if CC_TYPED_STEP_IS_VALID( step_2__d ) {
//             step_def__o->step_1 = step__d;
//             step_def__o->step_2 = step_2__d;
//             step_def__o->type = CC_TSDE_Two;
//             return true;
//         } else {
//             step_def__o->step_1 = step__d;
//             step_def__o->type = CC_TSDE_One;
//             return true;
//         }
//     }

//     if ( steps__d_n && *steps__d_n ) {
//         if ( CC_TYPED_STEP_IS_VALID( step__d ) ||
//              CC_TYPED_STEP_IS_VALID( step_2__d ) )
//                 return false;

//         step_def__o->steps = *steps__d_n; // Ownership transfer.
//         *steps__d_n = NULL;
//         return true;
//     }

//     return false;
// }

// bool cc_typed_step_def_unpack( CcTypedStepDef * step_def,
//                                CcTypedStep * step__o,
//                                CcTypedStep * step_2__o,
//                                CcTypedStepLink ** steps__o_w ) {
//     if ( !step_def ) return false;

//     if ( step_def->type == CC_TSDE_One ) {
//         if ( !step__o ) return false;

//         *step__o = step_def->step;
//         return true;
//     } else if ( step_def->type == CC_TSDE_Two ) {
//         if ( !step__o || !step_2__o ) return false;

//         *step__o = step_def->step_1;
//         *step_2__o = step_def->step_2;
//         return true;
//     } else if ( step_def->type == CC_TSDE_Many ) {
//         if ( !steps__o_w || *steps__o_w ) return false;

//         *steps__o_w = step_def->steps; // Weak output pointer, no ownership transfer.
//         return true;
//     }

//     return false;
// }

// // Convenience functions.
// bool cc_typed_step_def_one( CcTypedStep step, CcTypedStepDef * step_def__o ) {
//     return cc_typed_step_def( step, CC_TYPED_STEP_CAST_INVALID, NULL, step_def__o );
// }

// bool cc_typed_step_def_two( CcTypedStep step, CcTypedStep step_2, CcTypedStepDef * step_def__o ) {
//     return cc_typed_step_def( step, step_2, NULL, step_def__o );
// }

// bool cc_typed_step_def_many( CcTypedStepLink ** steps__n, CcTypedStepDef * step_def__o ) {
//     return cc_typed_step_def( CC_TYPED_STEP_CAST_INVALID, CC_TYPED_STEP_CAST_INVALID, steps__n, step_def__o );
// }
// todo :: DELETE
