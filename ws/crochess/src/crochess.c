// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_math.h"

#include "cc_version.h"
#include "cc_token.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_game.h"
#include "cc_rules.h"

#include "hlp_msgs.h"
#include "crochess.h"


char const CROCHESS_VERSION[] = "0.0.1.189:621+20221030.111706"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker


int main( void )
{
    print_app_intro( CC_LIB_VERSION, CROCHESS_VERSION );

    char * ret = NULL;
    char buffer[ BUFSIZ ];

    CcGame * game__a = cc_game__new( CC_GSE_Turn_Light, CC_VE_One, true );

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

        char const * token_start = NULL;
        char const * token_end = NULL;
        if ( !cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            continue;

        if ( cc_str_is_equal( token_start, token_end, "q", NULL, BUFSIZ ) ||
             cc_str_is_equal( token_start, token_end, "quit", NULL, BUFSIZ ) )
        {
            break;
        }
        else if ( cc_str_is_equal( token_start, token_end, "v", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "version", NULL, BUFSIZ ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_VERSION );
        }
        else if ( cc_str_is_equal( token_start, token_end, "a", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "about", NULL, BUFSIZ ) )
        {
            print_about_info();
        }
        else if ( cc_str_is_equal( token_start, token_end, "d", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "display", NULL, BUFSIZ ) )
        {
            cc_chessboard_print( game__a->chessboard, true );
        }
        else if ( cc_str_is_equal( token_start, token_end, "t", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "tags", NULL, BUFSIZ ) )
        {
            cc_chessboard_print( game__a->chessboard, false );
        }
        else if ( cc_str_is_equal( token_start, token_end, "l", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "list", NULL, BUFSIZ ) )
        {
            cc_moves_print( game__a->moves );
        }
        else if ( cc_str_is_equal( token_start, token_end, "m", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "move", NULL, BUFSIZ ) )
        {
            if ( cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            {
                char * an_str = cc_str_copy__new( token_start, token_end, CC_MAX_LEN_ZERO_TERMINATED );
                if ( !an_str )
                    continue;

                CcParseMsgs * pms__a = NULL;

                if ( cc_apply_move( an_str, game__a, &pms__a ) )
                {
                    // TODO :: TEMP :: uncomment (?)
                    // cc_chessboard_print( game__a->chessboard, true );
                }
                else
                {
                    CcParseMsgs * p = pms__a;
                    while ( p )
                    {
                        printf( "%s\n", p->msg );
                        p = p->next;
                    }
                }

                cc_parse_msgs_free_all( &pms__a );
            }
        }
        else if ( cc_str_is_equal( token_start, token_end, "n", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "new", NULL, BUFSIZ ) )
        {
            bool is_code = false;
            cc_char_8 code = CC_CHAR_8_EMPTY;

            if ( cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            {
                size_t len = cc_str_copy( token_start, token_end, CC_MAX_LEN_VARIANT_SYMBOL + 1, code, CC_SIZE_CHAR_8 );
                if ( len < 1 )
                    continue;

                CcVariantEnum ve = CC_VE_One;
                is_code = cc_variant_from_symbol( code, &ve );

                if ( is_code )
                {
                    if ( !cc_game_free_all( &game__a ) )
                        continue;

                    game__a = cc_game__new( CC_GSE_Turn_Light, ve, true );
                }
                else
                    print_new_code_invalid( code, CC_MAX_LEN_VARIANT_SYMBOL + 1 );
            }

            bool is_empty = cc_str_is_empty( code );
            if ( is_empty || ( !is_empty && is_code ) )
            {
                cc_chessboard_setup( game__a->chessboard );
                game__a->status = CC_GSE_Turn_Light;

                cc_chessboard_print( game__a->chessboard, true );
            }
        }
        else if ( cc_str_is_equal( token_start, token_end, "h", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "?", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "help", NULL, BUFSIZ ) )
        {
            if ( cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            {
                if ( token_start == token_end )
                    print_help();
                else if ( cc_str_is_equal( token_start, token_end, "q", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "quit", NULL, BUFSIZ ) )
                    print_help_quit();
                else if ( cc_str_is_equal( token_start, token_end, "d", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "display", NULL, BUFSIZ ) )
                    print_help_display();
                else if ( cc_str_is_equal( token_start, token_end, "t", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "tags", NULL, BUFSIZ ) )
                    print_help_tags();
                else if ( cc_str_is_equal( token_start, token_end, "a", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "about", NULL, BUFSIZ ) )
                    print_help_about();
                else if ( cc_str_is_equal( token_start, token_end, "v", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "version", NULL, BUFSIZ ) )
                    print_help_version();
                else if ( cc_str_is_equal( token_start, token_end, "n", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "new", NULL, BUFSIZ ) )
                    print_help_new();
                else
                    cc_str_print( token_start, token_end, BUFSIZ, "No help entry: '%s'.\n", "" );
            }
            else
                print_help();
        }
        else
        {
            printf( "Unknown: '%s'.\n", buffer );
            // fflush( stdout );
        }
    }

    cc_game_free_all( &game__a );

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
