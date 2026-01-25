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
                                           size_t * momentum_diff__o,
                                           CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_steps ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !momentum_diff__o ) return CC_MBE_Void;
    if ( !parse_msgs__iod ) return CC_MBE_Void;

    // if ( !path_ctx->cb_current ) return CC_MBE_Void; // Not needed, checked at [1].
    if ( !cc_path_context_is_legal( path_ctx, true, true ) ) return CC_MBE_Void; // [1].

    size_t mom_diff = cc_step_count( path_steps, true ); // Also checks if 1st step is initial, 2nd might be repositioning, all others are linked as next (i.e. immediate) steps.
    if ( mom_diff == 0 ) return CC_MBE_False;

    CcPosDesc init = path_ctx->ply_ctx.initial;

    if ( !cc_piece_has_same_type( ply->piece, init.piece, false ) ) return CC_MBE_False;

    CcStep * s = ply->steps;
    if ( !s ) return CC_MBE_Void;

    CcStep * ps = path_steps;
    if ( !ps ) return CC_MBE_Void;

    if ( ps->link == CC_SLTE_InitialPosition ) {
        if ( !CC_POS_IS_EQUAL( ps->field, init.pos ) ) return CC_MBE_False;

        if ( s->link == CC_SLTE_InitialPosition ) {
            if ( !CC_POS_IS_EQUAL( s->field, init.pos ) ) return CC_MBE_False;
            s = s->next;
        }

        ps = ps->next;
    }

    if ( ps->link == CC_SLTE_Reposition ) {
        if ( s->link == CC_SLTE_Reposition ) {
            if ( !CC_POS_IS_EQUAL( s->field, ps->field ) ) return CC_MBE_False;
            s = s->next;
        }

        ps = ps->next;
    }

    CcMaybeBoolEnum cumulative_result = CC_MBE_True;
    CcMaybeBoolEnum result = CC_MBE_True;

    while ( ps && s ) {
        if ( !CC_STEP_LINK_TYPE_IS_MOVEMENT( ps->link ) ) return CC_MBE_False;
        if ( !CC_STEP_LINK_TYPE_IS_MOVEMENT( s->link ) ) return CC_MBE_False;

        if ( CC_STEP_LINK_TYPE_IS_DESTINATION( ps->link ) ) {
            // No steps are allowed after destination.
            if ( ps->next ) return CC_MBE_False;
            if ( s->next ) return CC_MBE_False;
        }

        result = cc_step_is_congruent( s, ps );

        if ( result == CC_MBE_True ) {
            s = s->next;
        } else if ( result == CC_MBE_False ) {
            // Nothing to do here.
        } else // result is not valid --> error
            return result;

        cumulative_result = CC_MAYBE_BOOL_OR( result, cumulative_result );
        ps = ps->next;
    }

    if ( s && !ps ) return CC_MBE_Void; // Some of user notation has been left over --> error.

    return cumulative_result;
}

CcMaybeBoolEnum cc_path_cmp_compare_ply( CcPly * ply,
                                         CcPathNode * path_ply,
                                         CcPathContext * path_ctx,
                                         CcStep ** steps__o_a,
                                         size_t * momentum_diff__o,
                                         CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_ply ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !steps__o_a ) return CC_MBE_Void;
    if ( *steps__o_a ) return CC_MBE_Void;
    if ( !momentum_diff__o ) return CC_MBE_Void;
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

    CcMaybeBoolEnum result = cc_path_cmp_compare_steps( ply, steps__t, path_ctx, momentum_diff__o, parse_msgs__iod );

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
