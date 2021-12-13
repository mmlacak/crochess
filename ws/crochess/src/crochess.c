// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_version.h"
#include "cc_str_utils.h"
#include "cc_tokenizer.h"
#include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"

#include "hlp_msgs.h"
#include "crochess.h"


char const CROCHESS_VERSION[] = "0.0.2.139:338+20211213.105256"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker


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

        char const * first__w = NULL;
        char const * end__w = NULL;
        if ( !cc_token_iter_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
            continue;

        char * cmd__o = NULL;
        if ( !cc_str_copy_until_end_new( first__w, end__w, &cmd__o ) )
            continue;

        if ( ( !strcmp( "q", cmd__o ) ) || ( !strcmp( "quit", cmd__o ) ) )
        {
            free( cmd__o );
            break;
        }
        else if ( ( !strcmp( "v", cmd__o ) ) || ( !strcmp( "version", cmd__o ) ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_VERSION );
        }
        else if ( ( !strcmp( "a", cmd__o ) ) || ( !strcmp( "about", cmd__o ) ) )
        {
            print_about_info();
        }
        else if ( ( !strcmp( "d", cmd__o ) ) || ( !strcmp( "display", cmd__o ) ) )
        {
            cc_chessboard_print( cb, true );
        }
        else if ( ( !strcmp( "t", cmd__o ) ) || ( !strcmp( "tags", cmd__o ) ) )
        {
            cc_chessboard_print( cb, false );
        }
        else if ( ( !strcmp( "n", cmd__o ) ) || ( !strcmp( "new", cmd__o ) ) )
        {
            bool is_code = false;
            char * code = NULL;

            if ( cc_token_iter_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
            {
                if ( !cc_str_copy_until_end_new( first__w, end__w, &code ) )
                    continue;

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
        else if ( ( !strcmp( "h", cmd__o ) ) || ( !strcmp( "help", cmd__o ) ) || ( !strcmp( "?", cmd__o ) ) )
        {
            char * res = NULL;

            if ( cc_token_iter_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
                if ( !cc_str_copy_until_end_new( first__w, end__w, &res ) )
                    continue;

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
        else if ( !strcmp( "x", cmd__o ) )
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

        free( cmd__o );
    }

    free( cb );

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
