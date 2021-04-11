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


char const CROCHESS_VERSION[] = "0.0.0.15+20210411.013733"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker

int main(void)
{
    print_app_intro(LIBCROCHESS_VERSION, CROCHESS_VERSION);

    char * ret = NULL;
    char buffer[ BUFSIZ ];

    while ( true )
    {
        memset(buffer, 0, BUFSIZ);

        printf("> ");
        // fflush( stdout ); // Doesn't work from spawned process. Run directly from terminal works ok, even without fflush().

        ret = fgets(buffer, BUFSIZ, stdin);
        if ( !ret )
        {
            printf("Input error.\n");
            continue;
        }

        char * cmd = next_token_alloc(buffer, TOKEN_SEPARATORS_WHITEPSACE);
        if ( !cmd ) continue;

        if ( ( !strcmp("q", cmd) ) || ( !strcmp("quit", cmd) ) )
        {
            free( cmd );
            break;
        }
        else if ( ( !strcmp("v", cmd) ) || ( !strcmp("version", cmd) ) )
        {
            print_version_info(LIBCROCHESS_VERSION, CROCHESS_VERSION);
        }
        else if ( ( !strcmp("a", cmd) ) || ( !strcmp("about", cmd) ) )
        {
            print_about_info();
        }
        else if ( ( !strcmp("h", cmd) ) || ( !strcmp("help", cmd) ) || ( !strcmp("?", cmd) ) )
        {
            char * res = next_token_alloc(NULL, NULL);

            if ( !res ) print_help();
            else if ( ( !strcmp("q", res) ) || ( !strcmp("quit", res) ) ) print_help_quit();
            else if ( ( !strcmp("d", res) ) || ( !strcmp("display", res) ) ) print_help_display();
            else if ( ( !strcmp("t", res) ) || ( !strcmp("tags", res) ) ) print_help_tags();
            else if ( ( !strcmp("a", res) ) || ( !strcmp("about", res) ) ) print_help_about();
            else if ( ( !strcmp("v", res) ) || ( !strcmp("version", res) ) ) print_help_version();
            else if ( ( !strcmp("n", res) ) || ( !strcmp("new", res) ) ) print_help_new();
            else
            {
                printf("No help entry: '%s'.\n", res);
                // fflush( stdout );
            }

            free( res );
        }
        else
        {
            printf("Unknown: '%s'.\n", buffer);
            // fflush( stdout );
        }

        free( cmd );
    }

    printf("Bye, have a nice day!\n");
    // fflush( stdout );

    return 0;
}
