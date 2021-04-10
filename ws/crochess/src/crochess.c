// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libcrochess.h"
#include "tokenizer.h"

#include "crochess.h"
#include "hlp_msgs.h"


char const CROCHESS_VERSION[] = "0.0.0.10+20210410.055618"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker

int main(void)
{
    print_app_intro(LIBCROCHESS_VERSION, CROCHESS_VERSION);

    int ret = 0;

    char buffer[ BUFSIZ ];
    char const * const fmt = "%8191s"; // BUFSIZ == 8192

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
            ret = scanf(fmt, buffer);

            if ( ret == 0 ) print_help();
            else if ( ( !strcmp("q", buffer) ) || ( !strcmp("quit", buffer) ) ) print_help_quit();
            else if ( ( !strcmp("d", buffer) ) || ( !strcmp("display", buffer) ) ) print_help_display();
            else if ( ( !strcmp("t", buffer) ) || ( !strcmp("tags", buffer) ) ) print_help_tags();
            else if ( ( !strcmp("a", buffer) ) || ( !strcmp("about", buffer) ) ) print_help_about();
            else if ( ( !strcmp("v", buffer) ) || ( !strcmp("version", buffer) ) ) print_help_version();
            else if ( ( !strcmp("n", buffer) ) || ( !strcmp("new", buffer) ) ) print_help_new();
            else
            {
                printf("No help entry: '%s'.\n", buffer);
                // fflush( stdout );
            }

        }
        else
        {
            printf("Unknown: '%s'.\n", buffer);
            // fflush( stdout );
        }

// TODO :: fetch & print excess input
        flush_stdio();
    }

    printf("Bye, have a nice day!\n");
    // fflush( stdout );

    return 0;
}
