// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libcrochess.h"
#include "tokenizer.h"
#include "piece_type.h"
#include "chessboard.h"

#include "step.h"
#include "ply.h"
#include "move.h"
#include "do_move.h"

#include "crochess.h"
#include "hlp_msgs.h"


char const CROCHESS_VERSION[] = "0.0.0.59+20210502.180315"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker


int main( void )
{
    print_app_intro( LIBCROCHESS_VERSION, CROCHESS_VERSION );

    char * ret = NULL;
    char buffer[ BUFSIZ ];

    Chessboard * cb = cb_new_alx( BT_One, true );

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

        char * cmd = next_token_alx( buffer, TOKEN_SEPARATORS_WHITEPSACE );
        if ( !cmd ) continue;

        if ( ( !strcmp( "q", cmd ) ) || ( !strcmp( "quit", cmd ) ) )
        {
            free( cmd );
            break;
        }
        else if ( ( !strcmp( "v", cmd ) ) || ( !strcmp( "version", cmd ) ) )
        {
            print_version_info( LIBCROCHESS_VERSION, CROCHESS_VERSION );
        }
        else if ( ( !strcmp( "a", cmd ) ) || ( !strcmp( "about", cmd ) ) )
        {
            print_about_info();
        }
        else if ( ( !strcmp( "d", cmd ) ) || ( !strcmp( "display", cmd ) ) )
        {
            cb_print( cb, true );
        }
        else if ( ( !strcmp( "t", cmd ) ) || ( !strcmp( "tags", cmd ) ) )
        {
            cb_print( cb, false );
        }
        else if ( ( !strcmp( "n", cmd ) ) || ( !strcmp( "new", cmd ) ) )
        {
            bool is_code = false;
            char * code = next_token_alx( NULL, NULL );

            if ( code )
            {
                is_code = bt_is_code( code );

                if ( is_code )
                {
                    BoardType bt = bt_from_str( code );

                    free( cb );
                    cb = cb_new_alx( bt, true );
                }
                else
                {
                    print_new_code_invalid( code );
                }

                free( code );
            }

            if ( ( !code ) || ( code && is_code ) )
            {
                cb_setup( cb );
                cb_print( cb, true );
            }
        }
        else if ( ( !strcmp( "h", cmd ) ) || ( !strcmp( "help", cmd ) ) || ( !strcmp( "?", cmd ) ) )
        {
            char * res = next_token_alx( NULL, NULL );

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
            printf( "X: '%d'.\n", is_field_light(5, 2) );
            cb_clear( cb );
            cb_set_piece( cb, 5, 2, PT_LightBishop );
            cb_print( cb, true );
        }
        else if ( !strcmp( "y", cmd ) )
        {
            Chessboard * y = cb_new_alx( BT_One, false );

            cb_set_piece( y, 5, 2, PT_LightKnight );
            cb_print( y, true );

            Step * start = step_new_alx( SL_Start, 5, 2 );
            Step * dest = step_new_alx( SL_Destination, 6, 4 );
            start->next = dest;

            PlySideEffect pse = ply_side_effect_none();
            Ply * ply = ply_new_ply_alx( PT_LightKnight, start, pse );

            Move * move = mv_new_alx( ply, MS_None );

            do_move( y, move );
            cb_print( y, true );

            mv_free_move( &move );
            free( y );
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
