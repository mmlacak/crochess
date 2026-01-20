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
                                           CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_steps ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !parse_msgs__iod ) return CC_MBE_Void;



    return CC_MBE_Void; // TODO :: FIX
}

CcMaybeBoolEnum cc_path_cmp_compare_ply( CcPly * ply,
                                         CcPathNode * path_ply,
                                         CcPathContext * path_ctx,
                                         CcStep ** steps__o_a,
                                         CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_ply ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !steps__o_a ) return CC_MBE_Void;
    if ( *steps__o_a ) return CC_MBE_Void;
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

    CcMaybeBoolEnum result = cc_path_cmp_compare_steps( ply, steps__t, path_ctx, parse_msgs__iod );

    if ( result == CC_MBE_True ) {
        *steps__o_a = steps__t; // Ownership transfer, steps__t is now weak pointer.
    } else
        cc_step_free_all( &steps__t );

    return result;
}

CcMaybeBoolEnum cc_path_cmp_compare_all_plies( CcPly * ply,
                                               CcPathNode ** path_node__io,
                                               CcPathContext * path_ctx,
                                               CcParseMsg ** parse_msgs__iod ) {
    if ( !ply ) return CC_MBE_Void;
    if ( !path_node__io ) return CC_MBE_Void;
    if ( !*path_node__io ) return CC_MBE_Void;
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !parse_msgs__iod ) return CC_MBE_Void;

    CcPathNode * pn = *path_node__io;

    if ( !cc_path_node_iter_init( &pn ) )
        return CC_MBE_Void;

    CcMaybeBoolEnum loop = CC_MBE_Void;
    CcMaybeBoolEnum cond = CC_MBE_Void;
    CcMaybeBoolEnum result = CC_MBE_True;
    CcStep * last__a = NULL;
    CcStep * steps__a = NULL;

    while ( CC_MBE_True == ( loop = cc_path_node_iter_next( &pn ) ) ) {
        cond = cc_path_cmp_compare_ply( ply, pn, path_ctx, &steps__a, parse_msgs__iod );

        if ( cond == CC_MBE_Void ) break;

        result = CC_MAYBE_BOOL_OR( result, cond );

        if ( cond == CC_MBE_True ) {
            if ( last__a ) break; // Not unique path;

            last__a = steps__a;
        }
    }

    if ( loop == CC_MBE_Void || cond == CC_MBE_Void ) {
        cc_step_free_all( &last__a );
        cc_step_free_all( &steps__a );
        return CC_MBE_Void;
    } else
        return result;
}
