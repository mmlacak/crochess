// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#define __WITH_LINE_NOISE__ 1


#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef __WITH_LINE_NOISE__
#include "linenoise.h"
#endif // __WITH_LINE_NOISE__

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_math.h"

#include "cc_version.h"
#include "cc_token.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_game.h"
#include "cc_parse_msg.h"

#include "hlp_msgs.h"
#include "crochess.h"


char const CROCHESS_VERSION[] = "0.0.1.287:1466+20250809.195603"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker

#ifdef __WITH_LINE_NOISE__
char const CROCHESS_HISTORY_FILE_NAME[] = "history_crochess.txt";
#endif // __WITH_LINE_NOISE__


bool print_all_moves( CcMove * moves, bool is_score ) {
    if ( !moves ) return false;

    char const * move_str__a = cc_move_as_string__new( moves, is_score );

    printf( "%s", move_str__a );

    CC_FREE( move_str__a );

    return true;
}


int main( void ) {
    print_app_intro( CC_LIB_VERSION, CROCHESS_VERSION );

    char * line = NULL;

#ifdef __WITH_LINE_NOISE__
    char * ln_line__a = NULL;

    linenoiseHistoryLoad( CROCHESS_HISTORY_FILE_NAME );
    linenoiseHistorySetMaxLen( CROCHESS_HISTORY_LENGTH );
#else
    char * ret = NULL;
    char buffer[ BUFSIZ ];
#endif // __WITH_LINE_NOISE__

    CcGame * game__a = cc_game__new( CC_GSE_Turn_Light, CC_VE_One, true );

    while ( true ) {
#ifdef __WITH_LINE_NOISE__
        line = ln_line__a = linenoise( "> " );
#else
        memset( buffer, 0, BUFSIZ );

        printf( "> " );
        // fflush( stdout ); // Run directly from terminal works ok, even without fflush().

        ret = fgets( buffer, BUFSIZ, stdin );
        if ( !ret ) {
            printf( "Input error.\n" );
            continue;
        }

        line = buffer;
#endif // __WITH_LINE_NOISE__

        char const * token_start = NULL;
        char const * token_end = NULL;
        if ( !cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            continue;

        if ( cc_str_is_equal( token_start, token_end, "quit", NULL, BUFSIZ ) ) {
            break;
        } else if ( cc_str_is_equal( token_start, token_end, "version", NULL, BUFSIZ ) ) {
            print_version_info( CC_LIB_VERSION, CROCHESS_VERSION );
        } else if ( cc_str_is_equal( token_start, token_end, "about", NULL, BUFSIZ ) ) {
            print_about_info();
        } else if ( cc_str_is_equal( token_start, token_end, "d", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "display", NULL, BUFSIZ ) ) {
            cc_chessboard_print( game__a->chessboard, true );
        } else if ( cc_str_is_equal( token_start, token_end, "t", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "tags", NULL, BUFSIZ ) ) {
            cc_chessboard_print( game__a->chessboard, false );
        } else if ( cc_str_is_equal( token_start, token_end, "l", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "list", NULL, BUFSIZ ) ) {
            print_all_moves( game__a->moves, true );
        } else if ( cc_str_is_equal( token_start, token_end, "m", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "move", NULL, BUFSIZ ) ) {
            if ( cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) ) {
                char * an_str__a = cc_str_copy__new( token_start, token_end, CC_MAX_LEN_BUFFER );
                if ( !an_str__a )
                    continue;

                CcParseMsg * pm__a = NULL;

// todo
                // if ( cc_apply_move( an_str__a, game__a, &pm__a ) )
                // {
                //     // todo :: TEMP :: uncomment (?)
                //     // cc_chessboard_print( game__a->chessboard, true );
                // }
                // else
                // {
                //     CcParseMsg * p = pm__a;
                //     while ( p )
                //     {
                //         printf( "%s\n", p->msg );
                //         p = p->next;
                //     }
                // }
// todo

                cc_parse_msg_free_all( &pm__a );
                CC_FREE( an_str__a );
            }
        } else if ( cc_str_is_equal( token_start, token_end, "new", NULL, BUFSIZ ) ) {
            bool is_code = false;
            cc_char_8 code = CC_CHAR_8_EMPTY;

            if ( cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) ) {
                size_t len = cc_str_copy( token_start, token_end, CC_MAX_LEN_VARIANT_SYMBOL + 1, code, NULL, CC_SIZE_CHAR_8 );
                if ( len < 1 )
                    continue;

                CcVariantType ve = CC_VE_One;
                is_code = cc_variant_from_symbol( code, &ve );

                if ( is_code ) {
                    if ( !cc_game_free_all( &game__a ) )
                        continue;

                    game__a = cc_game__new( CC_GSE_Turn_Light, ve, true );
                } else
                    print_new_code_invalid( code, CC_MAX_LEN_VARIANT_SYMBOL + 1 );
            }

            bool is_empty = cc_str_is_empty( code, true );
            if ( is_empty || ( !is_empty && is_code ) ) {
                cc_chessboard_setup( game__a->chessboard );
                game__a->status = CC_GSE_Turn_Light;

                cc_chessboard_print( game__a->chessboard, true );
            }
        } else if ( cc_str_is_equal( token_start, token_end, "h", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "?", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "help", NULL, BUFSIZ ) ) {
            if ( cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) ) {
                if ( token_start == token_end )
                    print_help();
                else if ( cc_str_is_equal( token_start, token_end, "quit", NULL, BUFSIZ ) )
                    print_help_quit();
                else if ( cc_str_is_equal( token_start, token_end, "d", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "display", NULL, BUFSIZ ) )
                    print_help_display();
                else if ( cc_str_is_equal( token_start, token_end, "t", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "tags", NULL, BUFSIZ ) )
                    print_help_tags();
                else if ( cc_str_is_equal( token_start, token_end, "about", NULL, BUFSIZ ) )
                    print_help_about();
                else if ( cc_str_is_equal( token_start, token_end, "version", NULL, BUFSIZ ) )
                    print_help_version();
                else if ( cc_str_is_equal( token_start, token_end, "new", NULL, BUFSIZ ) )
                    print_help_new();
                else
                    CC_STR_PRINT( token_start, token_end, BUFSIZ, "No help entry: '%s'.\n", CC_MAX_LEN_BUFFER, NULL ); }
            else
                print_help();
        } else {
            printf( "Unknown: '%s'.\n", line );
            // fflush( stdout );
            continue;
        }

#ifdef __WITH_LINE_NOISE__
        linenoiseHistoryAdd( line );
        linenoiseHistorySave( CROCHESS_HISTORY_FILE_NAME );
#endif // __WITH_LINE_NOISE__
    }

    cc_game_free_all( &game__a );

#ifdef __WITH_LINE_NOISE__
    linenoiseFree( ln_line__a );
#endif // __WITH_LINE_NOISE__

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
