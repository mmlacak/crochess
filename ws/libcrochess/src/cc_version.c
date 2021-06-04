// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdio.h>

#include "cc_version.h"


char const CC_LIB_VERSION[] = "0.0.0.93+20210604.043333"; // source-new-libcrochess-version-major-minor-feature-commit+meta~breaks-place-marker


int cc_lib_test( void )
{
    printf( "\n" );
    printf( "Hello, from library!\n" );
    printf( "%s\n", CC_LIB_VERSION );
    printf( "Bye, from library!\n" );
    printf( "\n" );

    return 0;
}
