// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdio.h>

#include "libcrochess.h"


char LIBCROCHESS_VERSION[] = "0.0.0.2+20210408.044722"; // source-new-libcrochess-version-major-minor-feature-commit+meta~breaks-place-marker


int test(void)
{
    printf("\n");
    printf("Hello, from library!\n");
    printf("%s\n", LIBCROCHESS_VERSION);
    printf("Bye, from library!\n");
    printf("\n");

    return 0;
}
