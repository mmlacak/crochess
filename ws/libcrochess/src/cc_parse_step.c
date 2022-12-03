// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_side_effect.h"

#include "cc_parse_utils.h"
#include "cc_parse_step.h"


static bool cc_parse_step( char const * restrict step_start_an,
                           char const * restrict step_end_an,
                           CcGame * restrict game,
                           CcPos * restrict last_destination__iod,
                           CcStep ** restrict step__o,
                           CcChessboard ** restrict cb__io,
                           CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !game ) return false;
    if ( !last_destination__iod ) return false;
    if ( !step__o || *step__o ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__iod ) return false;

    CcStepLinkEnum sle = cc_starting_step_link( step_start_an );

    char const * s_an = step_start_an + cc_step_link_len( sle );

    CcPos pos = CC_POS_CAST_INVALID;
    char const * pos_end_an = NULL;

    if ( !cc_starting_pos( s_an, &pos, &pos_end_an ) )
    {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

        cc_parse_msg_append_format_if( parse_msgs__iod,
                                       CC_PMTE_Error,
                                       CC_MAX_LEN_ZERO_TERMINATED,
                                       "Error parsing step '%s'.\n",
                                       step_an__a );

        CC_FREE( step_an__a );

        return false;
    }

    CcPieceEnum step_piece = cc_chessboard_get_piece( *cb__io, pos.i, pos.j );
    CcSideEffect se = cc_side_effect_none();

    if ( !cc_starting_side_effect( pos_end_an, step_end_an, step_piece, &se ) )
    {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

        cc_parse_msg_append_format_if( parse_msgs__iod,
                                       CC_PMTE_Error,
                                       CC_MAX_LEN_ZERO_TERMINATED,
                                       "Error parsing side-effect, in step '%s'.\n",
                                       step_an__a );

        CC_FREE( step_an__a );

        return false;
    }

    CcStep * step__t = cc_step__new( sle, pos, se );
    if ( !step__t ) return false;

    *step__o = step__t;
    // step__t = NULL; // Not needed.

    return true;
}


bool cc_parse_steps( char const * restrict steps_start_an,
                     char const * restrict steps_end_an,
                     CcGame * restrict game,
                     CcPos * restrict last_destination__iod,
                     CcStep ** restrict steps__o,
                     CcChessboard ** restrict cb__io,
                     CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !steps_start_an ) return false;
    if ( !steps_end_an ) return false;
    if ( !game ) return false;
    if ( !steps__o || *steps__o ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__iod ) return false;

    char const * step_start_an = NULL;
    char const * step_end_an = NULL;

    while ( cc_step_iter( steps_start_an, steps_end_an, &step_start_an, &step_end_an ) )
    {
        CcStep * step__t = NULL;

cc_str_print( step_start_an, step_end_an, 0, "Step: '%s'.\n", 0, NULL ); // TODO :: DEBUG :: DELETE

        if ( !cc_parse_step( step_start_an, step_end_an, game, last_destination__iod,
                             &step__t,
                             cb__io,
                             parse_msgs__iod ) )
        {
            cc_step_free_all( &step__t );
            return false;
        }

        if ( !cc_step_extend_if( steps__o, &step__t ) )
        {
            cc_step_free_all( &step__t );
            return false;
        }
    }


    return true;
}
