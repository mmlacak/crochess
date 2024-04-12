// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

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
#include "cc_pos.h"
#include "cc_game.h"

#include "cc_parse_defs.h"
#include "cc_parse_utils.h"
#include "cc_parse_msg.h"
#include "cc_parse_move.h"
#include "cc_rules.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests_misc.h"
#include "tests_move.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.1.478:910+20240412.142714"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker

#ifdef __WITH_LINE_NOISE__
char const CROCHESS_TESTS_HISTORY_FILE_NAME[] = "history_tests.txt";
#endif // __WITH_LINE_NOISE__


int get_integer_from_cli_arg( char const * restrict str,
                              int default_num,
                              char const ** restrict first_io,
                              char const ** restrict end_io ) {
    int number = default_num;
    cc_char_16 num = CC_CHAR_16_EMPTY;

    if ( cc_iter_token( str, CC_TOKEN_SEPARATORS_WHITESPACE, first_io, end_io ) ) {
        if ( *first_io >= *end_io ) return default_num;

        size_t len = *end_io - *first_io;
        if ( len > CC_MAX_LEN_CHAR_16 - 1 ) return default_num;

        memcpy( num, *first_io, len );
        number = atoi( num );
    }

    return number;
}

bool print_all_moves( CcMove * restrict moves ) {
    if ( !moves ) return false;

    CcMove * m = moves;
    CcMove * l = NULL;
    CcMove * d = NULL;

    size_t i = 0;
    size_t index = 0;

    CC_REWIND( m );

    while ( m ) {
        if ( i++ % 2 == 0 ) {
            l = m;

            if ( !m->next ) {
                printf( "%lu %s ...\n", index+1, l->notation );
                break;
            }
        } else {
            d = m;
            printf( "%lu %s %s\n", ++index, l->notation, d->notation );
        }

        m = m->next;
    }

    return true;
}

char const * get_game_status_label( CcGameStatusEnum gse ) {
    switch ( gse ) {
        case CC_GSE_Turn_Light : return "Light player is on turn.";
        case CC_GSE_Turn_Dark : return "Dark player is on turn.";
        case CC_GSE_Win_Light : return "Light player has won.";
        case CC_GSE_Win_Dark : return "Dark player has won.";
        case CC_GSE_Draw : return "Draw.";

        case CC_GSE_None : return "Undetermined game status.";

        default : return "Unknown game status.";
    }
}


int main( void ) {
    print_app_intro( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );

// // DEBUG
//     // /* static */ char * foo = "xxx";
//     char * foo = malloc( 4 );
//     strncpy( foo, "xxx", 4 );
//     printf( "%s.\n", foo );

//     // foo = (char *)((int)foo + 1);
//     // printf( "%s.\n", foo );
//     printf( "%s.\n", ++foo );

//     CC_FREE( foo-1 );
//     // CC_FREE( foo );

//     printf( "%s.\n", foo );
// // DEBUG


// // DEBUG
//     printf("1: %d.\n", CC_MAX( 5, 11 ));
//     printf("2: %d.\n", CC_MAX( 11, 5 ));

//     printf("3: %d.\n", CC_MIN( 5, 11 ));
//     printf("4: %d.\n", CC_MIN( 11, 5 ));

//     printf("5: %f.\n", CC_MAX( 5.1, 11.2 ));
//     printf("6: %f.\n", CC_MAX( 11.3, 5.4 ));

//     printf("7: %f.\n", CC_MIN( 5.5, 11.6 ));
//     printf("8: %f.\n", CC_MIN( 11.7, 5.8 ));
// // DEBUG

    char * line = NULL;

#ifdef __WITH_LINE_NOISE__
    char * ln_line__a = NULL;

    linenoiseHistoryLoad( CROCHESS_TESTS_HISTORY_FILE_NAME );
    linenoiseHistorySetMaxLen( CROCHESS_TESTS_HISTORY_LENGTH );
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

        if ( cc_str_is_equal( token_start, token_end, "q", NULL, BUFSIZ ) ||
             cc_str_is_equal( token_start, token_end, "quit", NULL, BUFSIZ ) ) {
            break;
        } else if ( cc_str_is_equal( token_start, token_end, "version", NULL, BUFSIZ ) ) {
            print_version_info( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );
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
            print_all_moves( game__a->moves );
        } else if ( cc_str_is_equal( token_start, token_end, "m", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "move", NULL, BUFSIZ ) ) {
            if ( cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) ) {
                char * an_str = cc_str_copy__new( token_start, token_end, CC_MAX_LEN_ZERO_TERMINATED );
                if ( !an_str ) continue;

                CcParseMsg * pm__a = NULL;
                CcMove * move__a = NULL;

// TODO :: parse --> do apply
//
                if ( cc_parse_move( an_str, game__a, &move__a, &pm__a ) ) {
                    printf( "Move: '%s'.\n", move__a->notation );

                    // TODO :: TEMP :: uncomment (?)
                    // cc_chessboard_print( game__a->chessboard, true );

                    CC_FREE( move__a );
                } else {
                    CcParseMsg * p = pm__a;
                    while ( p ) {
                        printf( "%s\n", p->msg );
                        p = p->next;
                    }
                }
//
// TODO :: parse --> do apply

                cc_parse_msg_free_all( &pm__a );
            }
        } else if ( cc_str_is_equal( token_start, token_end, "p", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "player", NULL, BUFSIZ ) ) {
            CcGameStatusEnum gse = CC_GSE_None;

            if ( cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) ) {
                bool is_turn = false;
                char turn = *token_start;

                switch ( turn ) {
                    case 'l' :
                    case 'L' : gse = CC_GSE_Turn_Light; is_turn = true; break;

                    case 'w' :
                    case 'W' : gse = CC_GSE_Win_Light; is_turn = true; break;

                    case 'd' :
                    case 'D' : gse = CC_GSE_Turn_Dark; is_turn = true; break;

                    case 'b' :
                    case 'B' : gse = CC_GSE_Win_Dark; is_turn = true; break;

                    case '=' :
                    case '*' : gse = CC_GSE_Draw; is_turn = true; break;

                    case '-' :
                    case '?' : gse = CC_GSE_None; is_turn = true; break;

                    default : break;
                }

                if ( is_turn ) {
                    game__a->status = gse;
                    printf( "%s\n", get_game_status_label( game__a->status ) );
                } else {
                    print_new_player_invalid( turn );
                }
            } else {
                printf( "%s\n", get_game_status_label( game__a->status ) );
            }
        } else if ( cc_str_is_equal( token_start, token_end, "new", NULL, BUFSIZ ) ) {
            bool is_code = false;
            cc_char_8 code = CC_CHAR_8_EMPTY;

            if ( cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) ) {
                size_t len = cc_str_copy( token_start, token_end, CC_MAX_LEN_VARIANT_SYMBOL + 1,
                                          code,
                                          NULL,
                                          CC_SIZE_CHAR_8 );
                if ( len < 1 ) continue;

                CcVariantEnum ve = CC_VE_One;
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
        } else if ( cc_str_is_equal( token_start, token_end, "c", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "clear", NULL, BUFSIZ ) ) {
            bool is_empty = cc_str_is_empty( token_end, true );
            if ( is_empty ) {
                cc_chessboard_clear( game__a->chessboard );
            } else {
                CcChessboard * cb__t = cc_chessboard_clear_from_string__new( game__a->chessboard, token_end + 1 );

                if ( !cb__t )
                    printf( "Not valid chessboard setup.\n" );
                else {
                    cc_chessboard_free_all( &game__a->chessboard );
                    game__a->chessboard = cb__t;
                }
            }
        } else if ( cc_str_is_equal( token_start, token_end, "u", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "update", NULL, BUFSIZ ) ) {
            CcGame * game__t = cc_game_setup_from_string__new( token_end + 1, game__a );

            if ( !game__t )
                printf( "Not valid game setup.\n" );
            else {
                cc_game_free_all( &game__a );
                game__a = game__t;
            }
        } else if ( cc_str_is_equal( token_start, token_end, "s", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "setup", NULL, BUFSIZ ) ) {
            CcGame * game__t = cc_game_setup_from_string__new( token_end + 1, NULL );

            if ( !game__t )
                printf( "Not valid game setup.\n" );
            else {
                cc_game_free_all( &game__a );
                game__a = game__t;
            }
        } else if ( cc_str_is_equal( token_start, token_end, "tb", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "test_book", NULL, BUFSIZ ) ) {
        } else if ( cc_str_is_equal( token_start, token_end, "tp", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "test_parse", NULL, BUFSIZ ) ) {
        } else if ( cc_str_is_equal( token_start, token_end, "tm", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "test_move", NULL, BUFSIZ ) ) {
            int test_number = get_integer_from_cli_arg( line, 0, &token_start, &token_end );
            tests_move( test_number );
        } else if ( cc_str_is_equal( token_start, token_end, "tx", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "test_misc", NULL, BUFSIZ ) ) {
            int test_number = get_integer_from_cli_arg( line, 0, &token_start, &token_end );
            tests_misc( test_number );
#ifdef __WITH_LINE_NOISE__
        } else if ( cc_str_is_equal( token_start, token_end, "kc", NULL, BUFSIZ ) ||
                    cc_str_is_equal( token_start, token_end, "key_codes", NULL, BUFSIZ ) ) {
            linenoisePrintKeyCodes();
            break;
#endif // __WITH_LINE_NOISE__
        } else if ( cc_str_is_equal( token_start, token_end, "h", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "?", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "help", NULL, BUFSIZ ) ) {
            if ( cc_iter_token( line, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) ) {
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
                else if ( cc_str_is_equal( token_start, token_end, "about", NULL, BUFSIZ ) )
                    print_help_about();
                else if ( cc_str_is_equal( token_start, token_end, "version", NULL, BUFSIZ ) )
                    print_help_version();
                else if ( cc_str_is_equal( token_start, token_end, "p", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "player", NULL, BUFSIZ ) )
                    print_help_new_player();
                else if ( cc_str_is_equal( token_start, token_end, "new", NULL, BUFSIZ ) )
                    print_help_new();
                else if ( cc_str_is_equal( token_start, token_end, "c", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "clear", NULL, BUFSIZ ) )
                    print_help_clear();
                else if ( cc_str_is_equal( token_start, token_end, "u", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "update", NULL, BUFSIZ ) )
                    print_help_update();
                else if ( cc_str_is_equal( token_start, token_end, "s", NULL, BUFSIZ ) ||
                          cc_str_is_equal( token_start, token_end, "setup", NULL, BUFSIZ ) )
                    print_help_setup();
                else
                    cc_str_print( token_start, token_end, BUFSIZ, "No help entry: '%s'.\n", CC_MAX_LEN_ZERO_TERMINATED, NULL );
            } else
                print_help();
        } else {
            printf( "Unknown: '%s'.\n", line );
            // fflush( stdout );
            continue;
        }

#ifdef __WITH_LINE_NOISE__
        linenoiseHistoryAdd( line );
        linenoiseHistorySave( CROCHESS_TESTS_HISTORY_FILE_NAME );
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
