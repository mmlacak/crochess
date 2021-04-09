// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "libcrochess.h"

#include "crochess.h"
#include "hlp_msgs.h"


char const CROCHESS_VERSION[] = "0.0.0.6+20210409.203751"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker

int main(void)
{
    print_app_intro();

    int ret = 0;

    char buffer[ BUFSIZ ];
    char const * const fmt = "%8192s"; // BUFSIZ == 8192

    // test();

    while ( true )
    {
        memset(buffer, 0, BUFSIZ);

        printf("> ");
        // fflush( stdout ); // Doesn't work from spawned process. Run directly from terminal works ok, even without fflush().

        ret = scanf(fmt, buffer);
        if ( ret != 1 )
        {
            printf("Input error.\n");
            continue;
        }

        if ( ( !strcmp("q", buffer) ) || ( !strcmp("quit", buffer) ) ) break;
        else if ( ( !strcmp("v", buffer) ) || ( !strcmp("version", buffer) ) )
        {
            print_version_info(LIBCROCHESS_VERSION, CROCHESS_VERSION);
        }
        else if ( ( !strcmp("a", buffer) ) || ( !strcmp("about", buffer) ) )
        {
            print_about_info();
        }
        else if ( ( !strcmp("h", buffer) ) || ( !strcmp("help", buffer) ) )
        {
            print_help();
        }
        else
        {
            printf("Unknown: '%s'.\n", buffer);
            // fflush( stdout );
            continue;
        }
    }

    printf("Bye, have a nice day!\n");
    // fflush( stdout );

    return 0;
}
