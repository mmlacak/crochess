// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h> // printf :: TODO :: DEBUG :: DELETE

#include "cc_setup_misc.h"
#include "cc_side_effect.h"

#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"
#include "cc_parse_step.h"


static bool _cc_fail_with_msg_in_step( char const * msg_fmt,
                                       char const * step_start_an,
                                       char const * step_end_an,
                                       CcParseMsg ** parse_msgs__iod ) {
    char * step_str__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, step_str__a );
    CC_FREE( step_str__a );
    return false;
}

static bool _cc_parse_step( char const * step_start_an,
                            char const * step_end_an,
                            char const * steps_end_an,
                            bool is_turn_light,
                            cc_uint_t board_size,
                            bool is_first_step,
                            CcStep ** step__o,
                            CcParseMsg ** parse_msgs__iod ) {
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !steps_end_an ) return false;
    if ( !step__o || *step__o ) return false;
    if ( !parse_msgs__iod ) return false;

    char const * step_an = step_start_an;
    CcSideEffect se = cc_side_effect_none();

    if ( is_first_step ) {
        if ( cc_skip_disambiguation( step_an ) ) {
            CcPos da = CC_POS_CAST_INVALID;
            char const * da_end_an = NULL;

            if ( !cc_parse_pos( step_an, &da, &da_end_an ) )
                return _cc_fail_with_msg_in_step( "Error parsing disambiguation in step '%s'.\n", step_an, step_end_an, parse_msgs__iod );

            CcStep * da__t = cc_step__new( CC_SLTE_Init, da, se );
            if ( !da__t ) return false;

            if ( !cc_step_extend( step__o, &da__t ) ) {
                cc_step_free_all( &da__t );
                return false;
            }
            // da__t = NULL; // Not needed, ownership transferred.

            step_an = da_end_an;
            return (bool)( *step__o );
        }
    }

    CcStepLinkTypeEnum sle = CC_SLTE_None;
    CcMaybeBoolEnum result = cc_parse_step_link( step_an, steps_end_an, &sle );

    if ( result == CC_MBE_True )
        step_an += cc_step_link_len( sle );
    else if ( result == CC_MBE_False )
        return _cc_fail_with_msg_in_step( "Invalid step separator in step '%s'.\n", step_an, step_end_an, parse_msgs__iod );

    CcPos pos = CC_POS_CAST_INVALID;
    char const * pos_end_an = NULL;

    if ( !cc_parse_pos( step_an, &pos, &pos_end_an ) )
        return _cc_fail_with_msg_in_step( "Error parsing disambiguation in step '%s'.\n", step_an, step_end_an, parse_msgs__iod );

    if ( !cc_parse_side_effect( pos_end_an, step_an, step_end_an,
                                is_turn_light,
                                board_size,
                                &se,
                                parse_msgs__iod ) )
        return false;

    CcStep * step__t = cc_step__new( sle, pos, se );
    if ( !step__t ) return false;

    if ( !cc_step_extend( step__o, &step__t ) ) {
        cc_step_free_all( &step__t );
        return false;
    }
    // step__t = NULL; // Not needed, ownership transferred.

    return (bool)( *step__o );
}


bool cc_parse_steps( char const * steps_start_an,
                     char const * steps_end_an,
                     bool is_turn_light,
                     cc_uint_t board_size,
                     CcStep ** steps__o,
                     CcParseMsg ** parse_msgs__iod ) {
    if ( !steps_start_an ) return false;
    if ( !steps_end_an ) return false;
    if ( !steps__o || *steps__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return false;

    char const * step_start_an = NULL;
    char const * step_end_an = NULL;
    bool is_first_step = true;

    while ( cc_iter_step( steps_start_an, steps_end_an, &step_start_an, &step_end_an ) ) {
        CcStep * steps__t = NULL;

        cc_str_print( step_start_an, step_end_an, 0, "Step: '%s'.\n", 0, NULL ); // TODO :: DEBUG :: DELETE

        if ( !_cc_parse_step( step_start_an, step_end_an, steps_end_an, is_turn_light, board_size,
                              is_first_step,
                              &steps__t,
                              parse_msgs__iod ) ) {
            printf( "!_cc_parse_step\n" );  // TODO :: DEBUG :: DELETE

            cc_step_free_all( &steps__t );
            return false;
        }

        if ( !cc_step_extend( steps__o, &steps__t ) ) {
            cc_step_free_all( &steps__t );
            return false;
        }

        is_first_step = false;
    }

    // TODO :: DEBUG :: DELETE
    //
    {
        char * step_str__a = cc_step_all_to_string__new( *steps__o );

        cc_str_print( step_str__a, NULL, 0, "Steps: '%s'.\n", 0, NULL );

        CC_FREE( step_str__a );
    }
    //
    // TODO :: DEBUG :: DELETE

    return true;
}
