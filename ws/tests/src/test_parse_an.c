// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>

#include "cc_str_utils.h"
#include "cc_parse_utils.h"

#include "test_parse_an.h"

bool test_parse_ply_iter( char const * restrict move_an_str )
{
    if ( !move_an_str ) return false;

    printf( "\nMove: '%s'.\n", move_an_str );

    char const * first__w = NULL;
    char const * end__w = NULL;

    while ( cc_ply_iter( move_an_str, &first__w, &end__w ) )
    {
        printf( "1st: %p, 2nd: %p, ", (void *)first__w, (void *)end__w );

        if ( end__w > first__w )
        {
            printf( "ply: '" );
            cc_str_printf( first__w, end__w, CC_MAX_LEN_IGNORE ); // (size_t)( end__w - first__w )
            printf( "'" );
        }

        printf( "\n" );
    }

    return true;
}

bool test_parse( void )
{
    bool result = test_parse_ply_iter( "c8" );

    result = result && test_parse_ply_iter( "Gh2~Wb4|Wa24||Wr4@Bt6" );
    result = result && test_parse_ply_iter( "[Gj6-h2]~[Wh2-b4]|[Wm18-a24]||[Wx1-r4]@[Br4-t6]" );

    result = result && test_parse_ply_iter( "Sm15~Am11;;S..m17*..m19*.l20*.m21*.n20*.o21*" );
    result = result && test_parse_ply_iter( "[Sr14-m15]~[Am15-m11];;[Sm15..m17*..m19*.l20*.m21*.n20*.o21*]" );

    result = result && test_parse_ply_iter( "Sm15~Am11;;S.l16.m17*" );
    result = result && test_parse_ply_iter( "Sm15~Am11;;S..l16..m17*" );
    result = result && test_parse_ply_iter( "Sm15~Am11;;S.l14.k15.l16.k17.l18.m17*" );

    result = result && test_parse_ply_iter( "R&&e1" );
    result = result && test_parse_ply_iter( "R&&y1-q1" );
    result = result && test_parse_ply_iter( "Ba1*R&&" );
    result = result && test_parse_ply_iter( "Bb2-a1*R&&" );

    result = result && test_parse_ply_iter( "Re2~Wc2~b3" );
    result = result && test_parse_ply_iter( "Re2~Wc2~b3*" );
    result = result && test_parse_ply_iter( "Re2~Wc2~P::b3*N" );

    result = result && test_parse_ply_iter( "Hb14~We12@H..q16..k14..c18" );
    result = result && test_parse_ply_iter( "Hb14~We12@H..q16*..k14*..c18" );
    result = result && test_parse_ply_iter( "Hb14~We12@H,w18..q16*P..k14*N..c18" );

    result = result && test_parse_ply_iter( "Hb14~We12@@" );
    result = result && test_parse_ply_iter( "Hb14~We12@@P,B,R,R,N,B,N" );
    result = result && test_parse_ply_iter( "Hb14~We12@@Pq16,Bp14,Rd20,Rg6,Nk14,Bj12,Nd10" );

    return result;
}
