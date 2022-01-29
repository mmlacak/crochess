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


char const CROCHESS_VERSION[] = "0.0.2.213:412+20220129.064213"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker


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
        if ( !cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
            continue;

        if ( cc_str_is_equal( first__w, end__w, "q", NULL, BUFSIZ ) ||
             cc_str_is_equal( first__w, end__w, "quit", NULL, BUFSIZ ) )
        {
            break;
        }
        else if ( cc_str_is_equal( first__w, end__w, "v", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "version", NULL, BUFSIZ ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_VERSION );
        }
        else if ( cc_str_is_equal( first__w, end__w, "a", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "about", NULL, BUFSIZ ) )
        {
            print_about_info();
        }
        else if ( cc_str_is_equal( first__w, end__w, "d", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "display", NULL, BUFSIZ ) )
        {
            cc_chessboard_print( cb, true );
        }
        else if ( cc_str_is_equal( first__w, end__w, "t", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "tags", NULL, BUFSIZ ) )
        {
            cc_chessboard_print( cb, false );
        }
        else if ( cc_str_is_equal( first__w, end__w, "n", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "new", NULL, BUFSIZ ) )
        {
            bool is_code = false;
            char * code = NULL;

            if ( cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
            {
                code = cc_str_copy_new( first__w, end__w, BUFSIZ );
                if ( !code ) continue;

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
        else if ( cc_str_is_equal( first__w, end__w, "h", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "?", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "help", NULL, BUFSIZ ) )
        {
            char * res = NULL;

            if ( cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
            {
                res = cc_str_copy_new( first__w, end__w, BUFSIZ );
                if ( !res ) continue;
            }

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
        else if ( cc_str_is_equal( first__w, end__w, "x", NULL, BUFSIZ ) )
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
    }

    free( cb );

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
