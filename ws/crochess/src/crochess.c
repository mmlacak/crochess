// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_version.h"
#include "cc_tokenizer.h"
#include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"

#include "hlp_msgs.h"
#include "crochess.h"


char const CROCHESS_VERSION[] = "0.0.1.61:165+20210723.230722"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker


int main( void )
{
    print_app_intro( CC_LIB_VERSION, CROCHESS_VERSION );

    char * ret = NULL;
    char buffer[ BUFSIZ ];

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, true );

    while ( true )
    {
        memset( buffer, 0, BUFSIZ );

        printf( "> " );
        // fflush( stdout ); // Run directly from terminal works ok, even without fflush().

        ret = fgets( buffer, BUFSIZ, stdin );
        if ( !ret )
        {
            printf( "Input error.\n" );
            continue;
        }

        char * cmd = cc_next_token_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE );
        if ( !cmd ) continue;

        if ( ( !strcmp( "q", cmd ) ) || ( !strcmp( "quit", cmd ) ) )
        {
            free( cmd );
            break;
        }
        else if ( ( !strcmp( "v", cmd ) ) || ( !strcmp( "version", cmd ) ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_VERSION );
        }
        else if ( ( !strcmp( "a", cmd ) ) || ( !strcmp( "about", cmd ) ) )
        {
            print_about_info();
        }
        else if ( ( !strcmp( "d", cmd ) ) || ( !strcmp( "display", cmd ) ) )
        {
            cc_chessboard_print( cb, true );
        }
        else if ( ( !strcmp( "t", cmd ) ) || ( !strcmp( "tags", cmd ) ) )
        {
            cc_chessboard_print( cb, false );
        }
        else if ( ( !strcmp( "n", cmd ) ) || ( !strcmp( "new", cmd ) ) )
        {
            bool is_code = false;
            char * code = cc_next_token_new( NULL, NULL );

            if ( code )
            {
                is_code = cc_variant_str_is_symbol( code );

                if ( is_code )
                {
                    CcVariantEnum ve = cc_variant_from_symbol( code );

                    free( cb );
                    cb = cc_chessboard_new( ve, true );
                }
                else
                {
                    print_new_code_invalid( code );
                }

                free( code );
            }

            if ( ( !code ) || ( code && is_code ) )
            {
                cc_chessboard_setup( cb );
                cc_chessboard_print( cb, true );
            }
        }
        else if ( ( !strcmp( "h", cmd ) ) || ( !strcmp( "help", cmd ) ) || ( !strcmp( "?", cmd ) ) )
        {
            char * res = cc_next_token_new( NULL, NULL );

            if ( !res ) print_help();
            else if ( ( !strcmp( "q", res ) ) || ( !strcmp( "quit", res ) ) ) print_help_quit();
            else if ( ( !strcmp( "d", res ) ) || ( !strcmp( "display", res ) ) ) print_help_display();
            else if ( ( !strcmp( "t", res ) ) || ( !strcmp( "tags", res ) ) ) print_help_tags();
            else if ( ( !strcmp( "a", res ) ) || ( !strcmp( "about", res ) ) ) print_help_about();
            else if ( ( !strcmp( "v", res ) ) || ( !strcmp( "version", res ) ) ) print_help_version();
            else if ( ( !strcmp( "n", res ) ) || ( !strcmp( "new", res ) ) ) print_help_new();
            else
            {
                printf( "No help entry: '%s'.\n", res );
                // fflush( stdout );
            }

            free( res );
        }
        else if ( !strcmp( "x", cmd ) )
        {
            printf( "X: '%d'.\n", cc_is_field_light(5, 2) );
            cc_chessboard_clear( cb );
            cc_chessboard_set_piece( cb, 5, 2, CC_PE_LightBishop );
            cc_chessboard_print( cb, true );
        }
        else
        {
            printf( "Unknown: '%s'.\n", buffer );
            // fflush( stdout );
        }

        free( cmd );
    }

    free( cb );

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
