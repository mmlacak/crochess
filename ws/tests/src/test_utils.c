// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>

#include "test_utils.h"


char const TESTS_MOVE_TEST_SEPARATOR[] = " === === === === === === === === === === === === === === === === === === === === \n";
char const TESTS_MOVE_NOTATION_SEPARATOR[] = " ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... \n";
char const TESTS_MOVE_CHESSBOARD_SEPARATOR[] = " --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- \n";
char const TESTS_MOVE_MISC_SEPARATOR[] = " *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** \n";


TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move )
{
    TestPrints tp = { .do_print_chessboard = do_print_chessboard,
                      .do_print_move = do_print_move,
                      .format_move = format_move };
    return tp;
}


bool test_duplicates( CcMove const * const restrict moves )
{
    if ( !moves ) return false;

    printf( TESTS_MOVE_MISC_SEPARATOR );

    CcMove const * m = moves;
    while ( m )
    {
        printf( "%p\n", (void *)m );

        CcPly * p = m->plies;
        while ( p )
        {
            printf( "    %p\n", (void *)p );

            CcStep * s = p->steps;
            while ( s )
            {
                printf( "        %p\n", (void *)s );
                s = s->next;
            }

            p = p->next;
        }

        m = m->next;
    }

    printf( TESTS_MOVE_NOTATION_SEPARATOR );

    CcMove * dup__o = cc_move_duplicate_all_new( moves );
    if ( !dup__o ) return false;

    CcMove * d = dup__o;
    while ( d )
    {
        printf( "%p\n", (void *)d );

        CcPly * p = d->plies;
        while ( p )
        {
            printf( "    %p\n", (void *)p );

            CcStep * s = p->steps;
            while ( s )
            {
                printf( "        %p\n", (void *)s );
                s = s->next;
            }

            p = p->next;
        }

        d = d->next;
    }

    if ( !cc_move_free_all_moves( &dup__o ) ) return false;

    printf( TESTS_MOVE_MISC_SEPARATOR );

    return true;
}
