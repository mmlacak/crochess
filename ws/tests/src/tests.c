// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_version.h"
#include "cc_tokenizer.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"
#include "cc_gen_pos.h"
#include "cc_gen_steps.h"

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"
#include "cc_parse_msg.h"
#include "cc_parse_utils.h"
#include "cc_parse_move.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "test_utils.h"
#include "tests_do_move.h"
#include "tests_book_move.h"
#include "tests_parse_move.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.2.196:395+20211230.164754"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


TestMsg * test()
{
    TestMsg * test_msgs = NULL;

    test_msg_init_or_append( &test_msgs, TME_Debug, "just debugging", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append( &test_msgs, TME_Info, "just informing", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append( &test_msgs, TME_Warning, "just warning", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append( &test_msgs, TME_Error, "stepped into a turd", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append( &test_msgs, TME_Fatal, "it's a serious shit", __FILE__, __LINE__, __func__ );

    return test_msgs;
}


bool get_print_chessboard_from_cli_arg( char const * restrict str,
                                        char const ** restrict first_io,
                                        char const ** restrict end_io )
{
    bool do_print_chesboard = false;

    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, first_io, end_io ) )
    {
        if ( cc_str_is_equal( *first_io, *end_io, "1", NULL, 1 )
            || cc_str_is_equal( *first_io, *end_io, "true", NULL, 4 ) )
                do_print_chesboard = true;
    }

    return do_print_chesboard;
}

bool get_print_move_from_cli_arg( char const * restrict str,
                                  char const ** restrict first_io,
                                  char const ** restrict end_io )
{
    bool do_print_move = true;

    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, first_io, end_io ) )
    {
        if ( cc_str_is_equal( *first_io, *end_io, "0", NULL, 1 )
            || cc_str_is_equal( *first_io, *end_io, "false", NULL, 5 ) )
                do_print_move = true;
    }

    return do_print_move;
}

CcFormatMove get_format_move_from_cli_arg( char const * restrict str,
                                           char const ** restrict first_io,
                                           char const ** restrict end_io )
{
    CcFormatMove fm_user = cc_format_move_user( CC_FMSE_FormatOnlyCurrentMove );
    CcFormatMove fm_output = cc_format_move_output( CC_FMSE_FormatOnlyCurrentMove );
    CcFormatMove fm_debug = cc_format_move_debug( CC_FMSE_FormatOnlyCurrentMove );

    CcFormatMove format_move = fm_output;

    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, first_io, end_io ) )
    {
        if ( cc_str_is_equal( *first_io, *end_io, "u", NULL, 1 )
            || cc_str_is_equal( *first_io, *end_io, "user", NULL, 4 ) )
                format_move = fm_user;
        else if ( cc_str_is_equal( *first_io, *end_io, "d", NULL, 1 )
                || cc_str_is_equal( *first_io, *end_io, "debug", NULL, 5 ) )
                    format_move = fm_debug;
    }

    return format_move;
}

int get_integer_from_cli_arg( char const * restrict str,
                              int default_num,
                              char const ** restrict first_io,
                              char const ** restrict end_io )
{
    int number = default_num;
    char num[ 12 ] = { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', };

    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, first_io, end_io ) )
    {
        if ( *first_io >= *end_io ) return default_num;

        size_t len = *end_io - *first_io;
        if ( len > 11 ) return default_num;

        memcpy( num, *first_io, len );
        number = atoi( num );
    }

    return number;
}


int main( void )
{
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


    char * ret = NULL;
    char buffer[ BUFSIZ ];

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

        if ( cc_str_is_equal( first__w, end__w, "q", NULL, BUFSIZ ) ||
             cc_str_is_equal( first__w, end__w, "quit", NULL, BUFSIZ ) )
        {
            break;
        }
        else if ( cc_str_is_equal( first__w, end__w, "v", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "version", NULL, BUFSIZ ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );
        }
        else if ( cc_str_is_equal( first__w, end__w, "a", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "about", NULL, BUFSIZ ) )
        {
            print_about_info();
        }
        else if ( cc_str_is_equal( first__w, end__w, "b", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "book", NULL, BUFSIZ ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg( buffer, &first__w, &end__w );
            bool do_print_move = get_print_move_from_cli_arg( buffer, &first__w, &end__w );
            CcFormatMove format_move = get_format_move_from_cli_arg( buffer, &first__w, &end__w );
            int test_number = get_integer_from_cli_arg( buffer, 0, &first__w, &end__w );

            TestPrints tp = test_prints( do_print_chesboard, do_print_move, format_move );

            if ( ( test_number == 1 ) || ( test_number == 0 ) )
                if ( !test_book_move_scn_ct_03_define_step_ply( 1, tp ) )
                    printf( "Test test_book_move_scn_ct_03_define_step_ply() failed.\n" );
        }
        else if ( cc_str_is_equal( first__w, end__w, "t", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "test", NULL, BUFSIZ ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg( buffer, &first__w, &end__w );
            bool do_print_move = get_print_move_from_cli_arg( buffer, &first__w, &end__w );
            CcFormatMove format_move = get_format_move_from_cli_arg( buffer, &first__w, &end__w );
            int test_number = get_integer_from_cli_arg( buffer, 0, &first__w, &end__w );

            TestPrints tp = test_prints( do_print_chesboard, do_print_move, format_move );

            if ( ( test_number == 1 ) || ( test_number == 0 ) )
                if ( !test_do_move_single_ply( 1, tp ) )
                    printf( "Test test_do_move_single_ply() failed.\n" );

            if ( ( test_number == 2 ) || ( test_number == 0 ) )
                if ( !test_do_move_cascading_plies( 2, tp ) )
                    printf( "Test test_do_move_cascading_plies() failed.\n" );

            if ( ( test_number == 3 ) || ( test_number == 0 ) )
                if ( !test_do_move_castling( 3, tp ) )
                    printf( "Test test_do_move_castling() failed.\n" );

            if ( ( test_number == 4 ) || ( test_number == 0 ) )
                if ( !test_do_move_tag_and_promotion( 4, tp ) )
                    printf( "Test test_do_move_tag_and_promotion() failed.\n" );

            if ( ( test_number == 5 ) || ( test_number == 0 ) )
                if ( !test_do_move_conversion( 5, tp, false ) )
                    printf( "Test test_do_move_conversion( _, false ) failed.\n" );

            if ( ( test_number == 6 ) || ( test_number == 0 ) )
                if ( !test_do_move_conversion( 6, tp, true ) )
                    printf( "Test test_do_move_conversion( _, true ) failed.\n" );

            if ( ( test_number == 7 ) || ( test_number == 0 ) )
                if ( !test_do_move_demotion( 7, tp ) )
                    printf( "Test test_do_move_demotion() failed.\n" );


            if ( ( test_number == 8 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( 8, tp, false, false ) )
                    printf( "Test test_do_move_resurrection( _, false, false ) failed.\n" );

            if ( ( test_number == 9 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( 9, tp, false, true ) )
                    printf( "Test test_do_move_resurrection( _, false, true ) failed.\n" );

            if ( ( test_number == 10 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( 10, tp, true, false ) )
                    printf( "Test test_do_move_resurrection( _, true, false ) failed.\n" );

            if ( ( test_number == 11 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( 11, tp, true, true ) )
                    printf( "Test test_do_move_resurrection( _, true, true ) failed.\n" );


            if ( ( test_number == 12 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation( 12, tp, false ) )
                    printf( "Test test_do_move_teleportation( _, false ) failed.\n" );

            if ( ( test_number == 13 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation( 13, tp, true ) )
                    printf( "Test test_do_move_teleportation( _, true ) failed.\n" );


            if ( ( test_number == 14 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation_wave( 14, tp, false ) )
                    printf( "Test test_do_move_teleportation_wave( _, false ) failed.\n" );

            if ( ( test_number == 15 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation_wave( 15, tp, true ) )
                    printf( "Test test_do_move_teleportation_wave( _, true ) failed.\n" );


            if ( ( test_number == 16 ) || ( test_number == 0 ) )
                if ( !test_do_move_trance_journey( 16, tp, false ) )
                    printf( "Test test_do_move_trance_journey( _, false ) failed.\n" );

            if ( ( test_number == 17 ) || ( test_number == 0 ) )
                if ( !test_do_move_trance_journey( 17, tp, true ) )
                    printf( "Test test_do_move_trance_journey( _, true ) failed.\n" );

            printf( "Tests finished.\n" );
        }
        else if ( cc_str_is_equal( first__w, end__w, "x", NULL, BUFSIZ ) )
        {

            // char * user_an = "[Ng6]~[We5]~[Re8]";
            // char * user_an = "Ng6~[We5]~Re8";
            // char * user_an = "Ne6-a3";
            // char * user_an = "Hb14~We12@@P,B,R,R,N,B,N";

            // char * user_an = "Bi15~Wf12|Wr8~Np9";
            // char * user_an = "Bi15~Wf12||W";
            // char * user_an = "Hg10~Wh8@[H..h13<Bj19..f2<Nb6.p7..j19<Bl25-v5<P==p7]";
            char * user_an = "Hg10~Wh8@[H,j9..h13*B..f2*N.p7..j19-v5*P==]";

            // char * user_an = "Hig10~W10h8@[H..h13<Bj19..f2<Nb6.p7..9p17.rp17.r9p17..19p7.rp7.r19p7..9p7.rp7.r9p7..19p17.rp17.r19p17..j19<Bl25-v5<P==p7]";
            // char * user_an = "Sm15~Am11::S..m17*..m19*.l20*.m21*.n20*.o21*";
            // char * user_an = "[Sr14-m15]~[Am15-m11]::[Sm15..m17*..m19*.l20*.m21*.n20*.o21*]";
            // char * user_an = "Bi15~Wf12|Wr8|Na3@Np9||Ba3||K@@P,B,R,R,N,B,N@@@M::Sx7||";

            // Invalid.
            // char * user_an = "H..9p175.rp1q7.r9p1q7..9rp7.9r7p..195p7.r99p.r199p.r1X9p7..9X7.rX7.r9X7..19X17.rX17.r19X17";

            //
            // Test with AN from CLI.

            // <!> :: Uncomment CC_FREE() below, if this is active!
            // char * user_an = NULL;
            // if ( !cc_token_iter_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
            //     continue;


            //
            // Test reversing string.

            printf( "Original: '%s'.\n", user_an );

            char * reverse__o = cc_str_duplicate_new( user_an, true, BUFSIZ );
            printf( "Reverse: '%s'.\n", reverse__o );
            CC_FREE( reverse__o );
            reverse__o = NULL;

            reverse__o = cc_str_duplicate_new( user_an, true, BUFSIZ );
            printf( "Reverse: '%s'.\n", reverse__o );
            CC_FREE( reverse__o );
            reverse__o = NULL;

            // <!> :: Uncomment, if cc_token_iter_new() above is active!
            // CC_FREE( user_an );
            // user_an = NULL;

        }
        else if ( cc_str_is_equal( first__w, end__w, "y", NULL, BUFSIZ ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg( buffer, &first__w, &end__w );
            bool do_print_move = get_print_move_from_cli_arg( buffer, &first__w, &end__w );
            CcFormatMove format_move = get_format_move_from_cli_arg( buffer, &first__w, &end__w );
            int test_number = get_integer_from_cli_arg( buffer, 0, &first__w, &end__w );

            TestPrints tp = test_prints( do_print_chesboard, do_print_move, format_move );

            if ( ( test_number == 1 ) || ( test_number == 0 ) )
                if ( !test_parse_move_single_ply( tp ) )
                    printf( "Test test_parse_move_single_ply() failed.\n" );
        }
        else if ( cc_str_is_equal( first__w, end__w, "z", NULL, BUFSIZ ) )
        {
            // TestMsg * test_msgs = test();
            // test_msg_print_all( test_msgs, TME_Warning );
            // test_msg_free_all( &test_msgs );

            printf( TESTS_MOVE_TEST_SEPARATOR );

            CcChessboard * cb__a = cc_chessboard_new( CC_VE_One, false );
            if ( !cb__a ) return false;


            CcStep * steps_2__a = cc_step_none_new( CC_SLE_Start, 7, 7, CC_FSUE_Clarification_NoOutput );

            cc_step_none_append( steps_2__a, CC_SLE_Reposition, 9, 8, CC_FSUE_Clarification );

            CcSideEffect sse_2_1 = cc_side_effect_capture( CC_PE_LightBishop, CC_TE_None );
            cc_step_append( steps_2__a, CC_SLE_Distant, 7, 12, sse_2_1, CC_FSUE_User );

            CcSideEffect sse_2_2 = cc_side_effect_displacement( CC_PE_DarkKnight, false, 1, 5 );
            cc_step_append( steps_2__a, CC_SLE_Distant, 5, 1, sse_2_2, CC_FSUE_User );

            cc_step_none_append( steps_2__a, CC_SLE_Distant, 15, 6, CC_FSUE_Addition );

            CcStep * s = steps_2__a;
            while( s )
            {
                printf( "%p\n", (void *)s );
                s = s->next;
            }

            printf( TESTS_MOVE_NOTATION_SEPARATOR );


            CcStep * dup__a = cc_step_duplicate_all_new( steps_2__a );
            if ( !dup__a ) printf( "No dup!\n" );

            CcStep * d = dup__a;
            while( d )
            {
                printf( "%p\n", (void *)d );
                d = d->next;
            }

            cc_step_free_all_steps( &steps_2__a );

            cc_step_free_all_steps( &dup__a );

            printf( TESTS_MOVE_TEST_SEPARATOR );
        }
        else if ( cc_str_is_equal( first__w, end__w, "zz", NULL, BUFSIZ ) )
        {
            CcPos step = { -2, -7 };
            printf( "Start: (%d, %d)\n", step.i, step.j );

            for ( int k = 0; k < 10; ++k )
            {
                CcPos diff = { 1, 1 };
                cc_gen_pos( &step, diff, true );

                if ( CC_GEN_POS_UNICORN_STEP_IS_VALID( step ) )
                    printf( "Pos %d: (%d, %d)\n", k, step.i, step.j );
                else
                    printf( "Pos %d fail: (%d, %d)\n", k, step.i, step.j );
            }
        }
        else if ( cc_str_is_equal( first__w, end__w, "z0", NULL, BUFSIZ ) )
        {
            char * foo = cc_str_copy_new( "f:o:o", NULL, BUFSIZ );
            if ( !foo )
            {
                printf( "1: error cc_str_copy_new \n" );
                continue;
            }
            printf( "1: %s", foo );
            foo = cc_str_append_format_new( (char ** const)(&foo), BUFSIZ, "%c", ':' );
            if ( !foo )
            {
                printf( "1: error cc_str_append_format_new \n" );
                continue;
            }
            printf( " --> %s\n", foo );
            CC_FREE( foo );

            // --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

            char * bar = cc_str_copy_new( "b:a:r", NULL, BUFSIZ );
            if ( !bar )
            {
                printf( "2: error cc_str_copy_new \n" );
                continue;
            }
            printf( "2: %s", bar );
            bar = cc_str_append_format_new( (char ** const)(&bar), BUFSIZ, "%c", ';' );
            if ( !bar )
            {
                printf( "2: error \n" );
                continue;
            }
            printf( " --> %s\n", bar );
            CC_FREE( bar );

            // --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

            char * baz = cc_str_copy_new( "b:a:z", NULL, BUFSIZ );
            if ( !baz )
            {
                printf( "3: error cc_str_copy_new \n" );
                continue;
            }
            printf( "3: %s", baz );
            baz = cc_str_append_format_new( (char ** const)(&baz), BUFSIZ, "." );
            if ( !baz )
            {
                printf( "3: error \n" );
                continue;
            }
            printf( " --> %s\n", baz );
            CC_FREE( baz );
        }
        else if ( cc_str_is_equal( first__w, end__w, "z1", NULL, BUFSIZ ) )
        {
            char * con_1 = cc_str_concatenate_new( "Hello", "World!", BUFSIZ );
            printf( "1: %s.\n", con_1 );
            CC_FREE( con_1 );

            char * con_2 = cc_str_concatenate_new( "Hello", "World!", BUFSIZ );
            printf( "2: %s.\n", con_2 );
            CC_FREE( con_2 );

            char * con_3 = cc_str_concatenate_new( "Hello", "World!", 7 );
            printf( "3: %s.\n", con_3 );
            CC_FREE( con_3 );

            char * dup_4 = cc_str_duplicate_new( "Hello World!", false, BUFSIZ );
            printf( "4: %s.\n", dup_4 );
            CC_FREE( dup_4 );

            char * dup_5 = cc_str_duplicate_new( "Hello World!", false, 9 );
            printf( "5: %s.\n", dup_5 );
            CC_FREE( dup_5 );

            char * con_6 = cc_str_concatenate_new( NULL, "Hello World!", CC_MAX_LEN_IGNORE );
            printf( "6: %s.\n", con_6 );
            CC_FREE( con_6 );

            char * dup_7 = cc_str_concatenate_new( "Hello World!", NULL, BUFSIZ );
            printf( "7: %s.\n", dup_7 );
            CC_FREE( dup_7 );

            char * dup_8 = cc_str_concatenate_new( NULL, "Hello World!", 9 );
            printf( "8: %s.\n", dup_8 );
            CC_FREE( dup_8 );
        }
        else if ( cc_str_is_equal( first__w, end__w, "z2", NULL, BUFSIZ ) )
        {
            CcPosLink * pl__a = NULL;

            cc_pos_link_append_or_init( &pl__a, 1, 1 );
            cc_pos_link_append_or_init( &pl__a, 2, 2 );
            cc_pos_link_append_or_init( &pl__a, 3, 3 );
            cc_pos_link_append_or_init( &pl__a, 4, 4 );
            cc_pos_link_append_or_init( &pl__a, 5, 5 );
            cc_pos_link_append_or_init( &pl__a, 6, 6 );

            CcPosLink * x = pl__a;
            while ( x )
            {
                printf( "Pos: %d, %d (%p --> %p).\n", x->pos.i, x->pos.j, (void *)x, (void *)(x->next) );
                x = x->next;
            }

            if ( !cc_pos_link_free_all( &pl__a ) )
                continue;
        }
        else if ( cc_str_is_equal( first__w, end__w, "z3", NULL, BUFSIZ ) )
        {
            CcPly * ply__a = NULL;

            cc_ply_append_or_init( &ply__a, CC_PLE_Ply, CC_PE_LightRook, NULL );
            cc_ply_append_or_init( &ply__a, CC_PLE_Teleportation, CC_PE_DarkUnicorn, NULL );
            cc_ply_append_or_init( &ply__a, CC_PLE_FailedTeleportation, CC_PE_LightUnicorn, NULL );
            cc_ply_append_or_init( &ply__a, CC_PLE_TranceJourney, CC_PE_DarkPawn, NULL );
            cc_ply_append_or_init( &ply__a, CC_PLE_DualTranceJourney, CC_PE_LightPawn, NULL );
            cc_ply_append_or_init( &ply__a, CC_PLE_PawnSacrifice, CC_PE_DarkRook, NULL );

            CcPly * x = ply__a;
            while ( x )
            {
                printf( "Ply: %d, %d (%p --> %p).\n", x->link, x->piece, (void *)x, (void *)(x->next) );
                x = x->next;
            }

            if ( !cc_ply_free_all_plies( &ply__a ) )
                continue;
        }
        else if ( cc_str_is_equal( first__w, end__w, "z4", NULL, BUFSIZ ) )
        {
            CcStep * steps__a = NULL;
            CcSideEffect se = cc_side_effect_none();

            cc_step_append_or_init( &steps__a, CC_SLE_Start, 1, 1, se, CC_FSUE_Debug );
            cc_step_append_or_init( &steps__a, CC_SLE_Next, 2, 2, se, CC_FSUE_Debug );
            cc_step_append_or_init( &steps__a, CC_SLE_Distant, 3, 3, se, CC_FSUE_Debug );
            cc_step_append_or_init( &steps__a, CC_SLE_Next, 4, 4, se, CC_FSUE_Debug );
            cc_step_append_or_init( &steps__a, CC_SLE_Distant, 5, 5, se, CC_FSUE_Debug );
            cc_step_append_or_init( &steps__a, CC_SLE_Destination, 6, 6, se, CC_FSUE_Debug );

            CcStep * x = steps__a;
            while ( x )
            {
                printf( "Step: %d: %d, %d (%p --> %p).\n", x->link, x->i, x->j, (void *)x, (void *)(x->next) );
                x = x->next;
            }

            if ( !cc_step_free_all_steps( &steps__a ) )
                continue;
        }
        else if ( cc_str_is_equal( first__w, end__w, "z5", NULL, BUFSIZ ) )
        {
            char const * str = "a:b:c:d:e";
            char const * end = cc_str_end( str, BUFSIZ );
            printf( "%p -> %p, size: %lu ~= %lu\n", (void *)str, (void *)end, end - str, strlen( str ) );

            end = cc_str_end( str, 5 );
            printf( "%p -> %p, size: %lu ~= %lu\n", (void *)str, (void *)end, end - str, strlen( str ) );

            end = cc_str_end( str, 55 );
            printf( "%p -> %p, size: %lu ~= %lu\n", (void *)str, (void *)end, end - str, strlen( str ) );

            end = cc_str_end( str, -5 );
            printf( "%p -> %p, size: %lu ~= %lu\n", (void *)str, (void *)end, end - str, strlen( str ) );
        }
        else if ( cc_str_is_equal( first__w, end__w, "z6", NULL, BUFSIZ ) )
        {
            char const * str_1 = "a:b:c:d:e";
            char const * str_2 = "a:b:c:f:e";

            long long index = 0;

            printf( "--- --- --- \n" );

            for ( char const * s_1 = str_1, * s_2 = str_2 ;
                  ( *s_1 != '\0' ) && ( *s_2 != '\0' ) ;
                  ++s_1, ++s_2 )
            {
                if ( !cc_str_compare( s_1, NULL, s_2, NULL, CC_MAX_LEN_IGNORE, &index ) )
                    continue;

                printf( "\"%s\" == \"%s\": %lli.\n", s_1, s_2, index );
            }

            printf( "--- --- --- \n" );

            for ( char const * s_1 = str_1, * s_2 = str_2 ;
                  ( *s_1 != '\0' ) && ( *s_2 != '\0' ) ;
                  ++s_2 )
            {
                if ( !cc_str_compare( s_1, NULL, s_2, NULL, CC_MAX_LEN_IGNORE, &index ) )
                    continue;

                printf( "\"%s\" == \"%s\": %lli.\n", s_1, s_2, index );
            }

            printf( "--- --- --- \n" );

            for ( char const * s_1 = str_1, * s_2 = str_2 ;
                  ( *s_1 != '\0' ) && ( *s_2 != '\0' ) ;
                  ++s_1, ++s_2 )
            {
                if ( !cc_str_compare( s_1, s_1 + 3, s_2, s_2 + 3, CC_MAX_LEN_IGNORE, &index ) )
                    continue;

                printf( "+3: \"%s\" == \"%s\": %lli.\n", s_1, s_2, index );
            }

            printf( "--- --- --- \n" );

            for ( char const * s_1 = str_1, * s_2 = str_2 ;
                  ( *s_1 != '\0' ) && ( *s_2 != '\0' ) ;
                  ++s_1, ++s_2 )
            {
                if ( !cc_str_compare( s_1, NULL, s_2, NULL, 5, &index ) )
                    continue;

                printf( "max 5: \"%s\" == \"%s\": %lli.\n", s_1, s_2, index );
            }

            printf( "--- --- --- \n" );

            for ( char const * s_1 = str_1, * s_2 = str_2 ;
                  ( *s_1 != '\0' ) && ( *s_2 != '\0' ) ;
                  ++s_2 )
            {
                if ( !cc_str_compare( s_1, NULL, s_2, NULL, 5, &index ) )
                    continue;

                printf( "max 5: \"%s\" == \"%s\": %lli.\n", s_1, s_2, index );
            }

            printf( "--- --- --- \n" );

            for ( char const * s_1 = str_1, * s_2 = str_2 ;
                  ( *s_1 != '\0' ) && ( *s_2 != '\0' ) ;
                  ++s_1, ++s_2 )
            {
                if ( !cc_str_compare( s_1, s_1 + 3, s_2, s_2 + 3, 5, &index ) )
                    continue;

                printf( "+3: max 5: \"%s\" == \"%s\": %lli.\n", s_1, s_2, index );
            }

            printf( "--- --- --- \n" );

        }
        else if ( cc_str_is_equal( first__w, end__w, "z7", NULL, BUFSIZ ) )
        {
            char const * first = "a:b:c:d:e";
            char const * end_3 = first + 3;

            // Don't mind gcc warning, these here are intentional.
            // warning: array subscript xx is outside array bounds of ‘char[10]’ [-Warray-bounds]
            char const * end_99 = first + 99;
            char const * end__3 = first - 3;

            printf( "%zu ~ %zu ~ %zu\n", cc_str_len( first, NULL, CC_MAX_LEN_IGNORE ), cc_str_len( first, NULL, 5 ), cc_str_len( first, NULL, 33 ) );
            printf( "%zu ~ %zu ~ %zu\n", cc_str_len( first, end_3, CC_MAX_LEN_IGNORE ), cc_str_len( first, end_3, 5 ), cc_str_len( first, end_3, 33 ) );
            printf( "%zu ~ %zu ~ %zu\n", cc_str_len( first, end_99, CC_MAX_LEN_IGNORE ), cc_str_len( first, end_99, 5 ), cc_str_len( first, end_99, 33 ) );
            printf( "%zu ~ %zu ~ %zu\n", cc_str_len( first, end__3, CC_MAX_LEN_IGNORE ), cc_str_len( first, end__3, 5 ), cc_str_len( first, end__3, 33 ) );
        }
        else if ( cc_str_is_equal( first__w, end__w, "z8", NULL, BUFSIZ ) )
        {

            printf( TESTS_MOVE_TEST_SEPARATOR );

            // CcChessboard * cb__a = cc_chessboard_new( CC_VE_One, false );
            // if ( !cb__a ) return false;

            CcStep * steps_1__t = cc_step_none_new( CC_SLE_Start, 7, 7, CC_FSUE_Clarification_NoOutput );
            cc_step_none_append( steps_1__t, CC_SLE_Reposition, 9, 8, CC_FSUE_Clarification );

            CcPly * ply_2__a = cc_ply_new( CC_PLE_Ply, CC_PE_LightPegasus, &steps_1__t );


            CcSideEffect sse_2_1 = cc_side_effect_capture( CC_PE_LightBishop, CC_TE_None );
            CcStep * steps_2__t = cc_step_new( CC_SLE_Distant, 7, 12, sse_2_1, CC_FSUE_User );

            CcSideEffect sse_2_2 = cc_side_effect_displacement( CC_PE_DarkKnight, false, 1, 5 );
            cc_step_append( steps_2__t, CC_SLE_Distant, 5, 1, sse_2_2, CC_FSUE_User );

            cc_step_none_append( steps_2__t, CC_SLE_Distant, 15, 6, CC_FSUE_Addition );

            cc_ply_append( ply_2__a, CC_PLE_Teleportation, CC_PE_LightPyramid, &steps_2__t );

            CcPly * p = ply_2__a;
            while( p )
            {
                printf( "%p\n", (void *)p );

                CcStep * s = p->steps;
                while ( s )
                {
                    printf( "    %p\n", (void *)s );
                    s = s->next;
                }

                p = p->next;
            }


            printf( TESTS_MOVE_NOTATION_SEPARATOR );

            CcPly * dup__a = cc_ply_duplicate_all_new( ply_2__a );
            if ( !dup__a ) printf( "No duplicate!\n" );

            CcPly * d = dup__a;
            while( d )
            {
                printf( "%p\n", (void *)d );

                CcStep * s = d->steps;
                while ( s )
                {
                    printf( "    %p\n", (void *)s );
                    s = s->next;
                }

                d = d->next;
            }

            cc_ply_free_all_plies( &ply_2__a );

            cc_ply_free_all_plies( &dup__a );

            printf( TESTS_MOVE_TEST_SEPARATOR );
        }
        else
        {
            printf( "Unknown: '%s'.\n", buffer );
            // fflush( stdout );
        }
    }

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
