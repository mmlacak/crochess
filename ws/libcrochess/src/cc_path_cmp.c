// Copyright (c) 2026 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_pos_utils.h"
#include "cc_checks.h"
#include "cc_path_utils.h"
#include "cc_path_cmp.h"


CcMaybeBoolEnum cc_path_cmp_compare_steps( CcPly * ply,
                                           CcStep * path_steps,
                                           CcPathContext * path_ctx,
                                           size_t * momentum__o,
                                           CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_steps ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !momentum__o ) return CC_MBE_Void;
    if ( !parse_msgs__iod ) return CC_MBE_Void;

    // ply->piece


    return CC_MBE_Void; // TODO :: FIX
}

CcMaybeBoolEnum cc_path_cmp_compare_ply( CcPly * ply,
                                         CcPathNode * path_ply,
                                         CcPathContext * path_ctx,
                                         CcStep ** steps__o_a,
                                         size_t * momentum__o,
                                         CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_ply ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !steps__o_a ) return CC_MBE_Void;
    if ( *steps__o_a ) return CC_MBE_Void;
    if ( !momentum__o ) return CC_MBE_Void;
    if ( !parse_msgs__iod ) return CC_MBE_Void;

    CcPathLink * pl__a = NULL;

    if ( !cc_path_link_from_nodes( path_ply, &pl__a ) )
        return CC_MBE_Void;

    CcStep * steps__t = NULL;

    if ( !cc_path_link_to_steps( pl__a, &steps__t ) ) {
        cc_path_link_free_all( &pl__a );
        return CC_MBE_Void;
    }

    cc_path_link_free_all( &pl__a );

    CcMaybeBoolEnum result = cc_path_cmp_compare_steps( ply, steps__t, path_ctx, momentum__o, parse_msgs__iod );

    if ( result == CC_MBE_True ) {
        if ( steps__t )
            *steps__o_a = steps__t; // Ownership transfer, steps__t is now weak pointer.
        else {
            cc_step_free_all( &steps__t ); // Result was true, but no steps --> error.
            return CC_MBE_Void;
        }
    } else
        cc_step_free_all( &steps__t );

    return result;
}

CcMaybeBoolEnum cc_path_cmp_compare_all_plies( CcPly * ply,
                                               CcPathNode ** path_node__io,
                                               CcPathContext * path_ctx,
                                               CcStep ** shortest__o_a,
                                               CcStep ** longest__o_a,
                                               CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_node__io ) return CC_MBE_Void;
    if ( !*path_node__io ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;

    if ( !shortest__o_a ) return CC_MBE_Void;
    if ( *shortest__o_a ) return CC_MBE_Void;

    if ( !longest__o_a ) return CC_MBE_Void;
    if ( *longest__o_a ) return CC_MBE_Void;

    if ( !parse_msgs__iod ) return CC_MBE_Void;

    CcPathNode * pn = *path_node__io;

    if ( !cc_path_node_iter_init( &pn ) )
        return CC_MBE_Void;

    CcMaybeBoolEnum loop = CC_MBE_Void;
    CcMaybeBoolEnum cmp = CC_MBE_Void;
    CcMaybeBoolEnum result = CC_MBE_False; // <!> True here would mask when comparison fails, because results are OR'ed.

    size_t momentum = 0;
    CcStep * steps__t = NULL;

    size_t smallest = 0;
    CcStep * shortest__t = NULL;

    size_t largest = 0;
    CcStep * longest__t = NULL;

    while ( CC_MBE_True == ( loop = cc_path_node_iter_next( &pn ) ) ) {
        cmp = cc_path_cmp_compare_ply( ply, pn, path_ctx, &steps__t, &momentum, parse_msgs__iod );

        if ( cmp == CC_MBE_Void ) break;

        result = CC_MAYBE_BOOL_OR( result, cmp );

        if ( cmp == CC_MBE_True ) {
            if ( momentum == 0 || !steps__t ) {
                cmp = CC_MBE_Void; // Compare was true, but results are invalid --> error.
                break;
            }

            if ( momentum < smallest ) {
                smallest = momentum;
                shortest__t = steps__t;
            } else if ( largest < momentum ) {
                largest = momentum;
                longest__t = steps__t;
            }
        }
    }

    if ( loop == CC_MBE_Void || cmp == CC_MBE_Void ) {
        cc_step_free_all( &longest__t );
        cc_step_free_all( &shortest__t );
        cc_step_free_all( &steps__t );
        return CC_MBE_Void;
    } else {
        if ( shortest__t ) {
            *shortest__o_a = shortest__t; // Ownership transfer, shortest__t is now weak pointer.
        }

        if ( longest__t ) {
            *longest__o_a = longest__t; // Ownership transfer, longest__t is now weak pointer.
        }

        return result;
    }
}
