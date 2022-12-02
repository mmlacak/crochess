// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_utils.h"
#include "cc_parse_step.h"


static bool cc_parse_step( char const * restrict step_start_an,
                           char const * restrict step_end_an,
                           CcGame * restrict game,
                           CcPos * restrict last_destination__iod,
                           CcStep ** restrict step__o,
                           CcChessboard ** restrict cb__io,
                           CcParseMsg ** restrict parse_msgs__io )
{
}


bool cc_parse_steps( char const * restrict ply_start_an,
                     char const * restrict ply_end_an,
                     CcGame * restrict game,
                     CcPos * restrict last_destination__iod,
                     CcStep ** restrict steps__o,
                     CcChessboard ** restrict cb__io,
                     CcParseMsg ** restrict parse_msgs__io )
{
    if ( !ply_start_an ) return false;
    if ( !ply_end_an ) return false;
    if ( !game ) return false;
    if ( !steps__o || *steps__o ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__io ) return false;

    char const * step_start_an = NULL;
    char const * step_end_an = NULL;

    while ( cc_step_iter( ply_start_an, ply_end_an, &step_start_an, &step_end_an ) )
    {
        CcStep * step__t = NULL;

        if ( !cc_parse_step( step_start_an, step_end_an, game, last_destination__iod,
                             &step__t,
                             cb__io,
                             parse_msgs__io ) )
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
