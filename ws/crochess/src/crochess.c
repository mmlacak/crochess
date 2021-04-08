// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdio.h>

#include "libcrochess.h"

#include "crochess.h"


int main(void)
{
    printf("\n");
    printf("Hello world!\n");
    printf("Library = %s\n", LIBCROCHESS_VERSION);
    printf("Application = %s\n", CROCHESS_VERSION);
    printf("Bye world!\n");
    printf("\n");

    test();

    return 0;
}
