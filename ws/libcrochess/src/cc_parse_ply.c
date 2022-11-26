// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include <stdio.h> // TEMP

#include "cc_str_utils.h" // TEMP

#include "cc_parse_utils.h"
#include "cc_parse_ply.h"


bool cc_parse_plies( char const * restrict plies_an,
                     CcGame * restrict game,
                     CcPly ** restrict plies__o,
                     CcParseMsg ** restrict parse_msgs__io )
{
    if ( !plies_an ) return false;
    if ( !game ) return false;
    if ( !plies__o ) return false;
    if ( *plies__o ) return false;
    if ( !parse_msgs__io ) return false;

    char const * ply_start_an = NULL;
    char const * ply_end_an = NULL;

    while ( cc_ply_iter( plies_an, &ply_start_an, &ply_end_an ) )
    {
        CC_STR_PRINT_IF_INFO( ply_start_an, ply_end_an, CC_MAX_LEN_ZERO_TERMINATED, "Ply: '%s'.\n", CC_MAX_LEN_ZERO_TERMINATED, NULL );

        // printf( "Ply: '%s'.\n",  );
    }


    return true;
}
