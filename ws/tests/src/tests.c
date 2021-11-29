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

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_pos.h"
#include "cc_move.h"
#include "cc_parse_msg.h"
#include "cc_parse_utils.h"
#include "cc_parse_move.h"
#include "cc_gen_steps.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "test_utils.h"
#include "tests_do_move.h"
#include "tests_book_move.h"
#include "tests_parse_move.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.2.126:325+20211129.155803"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


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


bool get_print_chessboard_from_cli_arg( char const * const restrict str )
{
    bool do_print_chesboard = false;

    char * dpcb__o = NULL;
    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, &dpcb__o, false ) )
        do_print_chesboard = ( ( !strncmp( dpcb__o, "1", 1 ) ) || ( !strncmp( dpcb__o, "true", 4 ) ) ) ? true : false;

    free( dpcb__o );
    dpcb__o = NULL;

    return do_print_chesboard;
}

bool get_print_move_from_cli_arg( char const * const restrict str )
{
    bool do_print_move = true;

    char * dpm__o = NULL;
    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, &dpm__o, false ) )
        do_print_move = ( ( !strncmp( dpm__o, "0", 1 ) ) || ( !strncmp( dpm__o, "false", 5 ) ) ) ? false : true;

    free( dpm__o );
    dpm__o = NULL;

    return do_print_move;
}

CcFormatMove get_format_move_from_cli_arg( char const * const restrict str )
{
    CcFormatMove fm_user = cc_format_move_user( CC_FMSE_FormatOnlyCurrentMove );
    CcFormatMove fm_output = cc_format_move_output( CC_FMSE_FormatOnlyCurrentMove );
    CcFormatMove fm_debug = cc_format_move_debug( CC_FMSE_FormatOnlyCurrentMove );

    CcFormatMove format_move = fm_output;

    char * fm__o = NULL;
    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, &fm__o, false ) )
        format_move = ( ( !strncmp( fm__o, "u", 1 ) ) || ( !strncmp( fm__o, "user", 4 ) ) ) ? fm_user
                    : ( ( !strncmp( fm__o, "d", 1 ) ) || ( !strncmp( fm__o, "debug", 5 ) ) ) ? fm_debug
                    : fm_output;

    free( fm__o );
    fm__o = NULL;

    return format_move;
}

int get_test_number_from_cli_arg( char const * const restrict str )
{
    int test_number = 0; // all tests

    char * tn__o = NULL;
    if ( cc_token_iter_new( str, CC_TOKEN_SEPARATORS_WHITESPACE, &tn__o, false ) )
        test_number = atoi( tn__o );

    free( tn__o );
    tn__o = NULL;

    return test_number;
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

//     free( foo-1 );
//     // free( foo );

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


// // DEBUG
//     char * con_1 = cc_str_concatenate_new( "Hello", "World!" );
//     printf( "1: %s.\n", con_1 );
//     free( con_1 );

//     char * con_2 = cc_str_concatenate_len_new( "Hello", "World!", BUFSIZ );
//     printf( "2: %s.\n", con_2 );
//     free( con_2 );

//     char * con_3 = cc_str_concatenate_len_new( "Hello", "World!", 7 );
//     printf( "3: %s.\n", con_3 );
//     free( con_3 );

//     char * dup_4 = cc_str_duplicate_len_new( "Hello World!", false, BUFSIZ );
//     printf( "4: %s.\n", dup_4 );
//     free( dup_4 );

//     char * dup_5 = cc_str_duplicate_len_new( "Hello World!", false, 9 );
//     printf( "5: %s.\n", dup_5 );
//     free( dup_5 );

//     char * con_6 = cc_str_concatenate_new( NULL, "Hello World!" );
//     printf( "6: %s.\n", con_6 );
//     free( con_6 );

//     char * dup_7 = cc_str_concatenate_len_new( "Hello World!", NULL, BUFSIZ );
//     printf( "7: %s.\n", dup_7 );
//     free( dup_7 );

//     char * dup_8 = cc_str_concatenate_len_new( NULL, "Hello World!", 9 );
//     printf( "8: %s.\n", dup_8 );
//     free( dup_8 );
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

        char * cmd = NULL;
        if ( !cc_token_iter_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &cmd, true ) )
            continue;

        if ( ( !strcmp( "q", cmd ) ) || ( !strcmp( "quit", cmd ) ) )
        {
            free( cmd );
            break;
        }
        else if ( ( !strcmp( "v", cmd ) ) || ( !strcmp( "version", cmd ) ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );
        }
        else if ( ( !strcmp( "a", cmd ) ) || ( !strcmp( "about", cmd ) ) )
        {
            print_about_info();
        }
        else if ( ( !strcmp( "b", cmd ) ) || ( !strcmp( "book", cmd ) ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg( buffer );
            bool do_print_move = get_print_move_from_cli_arg( buffer );
            CcFormatMove format_move = get_format_move_from_cli_arg( buffer );
            int test_number = get_test_number_from_cli_arg( buffer );

            TestPrints tp = test_prints( do_print_chesboard, do_print_move, format_move );

            if ( ( test_number == 1 ) || ( test_number == 0 ) )
                if ( !test_book_move_scn_ct_03_define_step_ply( 1, tp ) )
                    printf( "Test test_book_move_scn_ct_03_define_step_ply() failed.\n" );
        }
        else if ( ( !strcmp( "t", cmd ) ) || ( !strcmp( "test", cmd ) ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg( buffer );
            bool do_print_move = get_print_move_from_cli_arg( buffer );
            CcFormatMove format_move = get_format_move_from_cli_arg( buffer );
            int test_number = get_test_number_from_cli_arg( buffer );

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
        else if ( !strcmp( "x", cmd ) )
        {

            // char const * const user_an = "[Ng6]~[We5]~[Re8]";
            // char const * const user_an = "Ng6~[We5]~Re8";
            // char const * const user_an = "Ne6-a3";
            // char const * const user_an = "Hb14~We12@@P,B,R,R,N,B,N";

            // char const * const user_an = "Bi15~Wf12|Wr8~Np9";
            // char const * const user_an = "Bi15~Wf12||W";
            // char const * const user_an = "Hg10~Wh8@[H..h13<Bj19..f2<Nb6.p7..j19<Bl25-v5<P==p7]";
            char const * const user_an = "Hg10~Wh8@[H,j9..h13*B..f2*N.p7..j19-v5*P==]";

            // char const * const user_an = "Hig10~W10h8@[H..h13<Bj19..f2<Nb6.p7..9p17.rp17.r9p17..19p7.rp7.r19p7..9p7.rp7.r9p7..19p17.rp17.r19p17..j19<Bl25-v5<P==p7]";
            // char const * const user_an = "Sm15~Am11::S..m17*..m19*.l20*.m21*.n20*.o21*";
            // char const * const user_an = "[Sr14-m15]~[Am15-m11]::[Sm15..m17*..m19*.l20*.m21*.n20*.o21*]";
            // char const * const user_an = "Bi15~Wf12|Wr8|Na3@Np9||Ba3||K@@P,B,R,R,N,B,N@@@M::Sx7||";

            // Invalid.
            // char const * const user_an = "H..9p175.rp1q7.r9p1q7..9rp7.9r7p..195p7.r99p.r199p.r1X9p7..9X7.rX7.r9X7..19X17.rX17.r19X17";

            //
            // Test with AN from CLI.

            // <!> :: Uncomment free() below, if this is active!
            // char * user_an = NULL;
            // if ( !cc_token_iter_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &user_an, false ) )
            //     continue;


            //
            // Test reversing string.

            printf( "Original: '%s'.\n", user_an );

            char * reverse__o = cc_str_duplicate_len_new( user_an, true, BUFSIZ );
            printf( "Reverse: '%s'.\n", reverse__o );
            free( reverse__o );
            reverse__o = NULL;

            reverse__o = cc_str_duplicate_new( user_an, true );
            printf( "Reverse: '%s'.\n", reverse__o );
            free( reverse__o );
            reverse__o = NULL;

            // <!> :: Uncomment, if cc_token_iter_new() above is active!
            // free( user_an );
            // user_an = NULL;

        }
        else if ( !strcmp( "y", cmd ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg( buffer );
            bool do_print_move = get_print_move_from_cli_arg( buffer );
            CcFormatMove format_move = get_format_move_from_cli_arg( buffer );
            int test_number = get_test_number_from_cli_arg( buffer );

            TestPrints tp = test_prints( do_print_chesboard, do_print_move, format_move );

            if ( ( test_number == 1 ) || ( test_number == 0 ) )
                if ( !test_parse_move_single_ply( tp ) )
                    printf( "Test test_parse_move_single_ply() failed.\n" );
        }
        else if ( !strcmp( "z", cmd ) )
        {
            // TestMsg * test_msgs = test();
            // test_msg_print_all( test_msgs, TME_Warning );
            // test_msg_free_all( &test_msgs );

            printf( TESTS_MOVE_TEST_SEPARATOR );

            CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
            if ( !cb__o ) return false;


            CcStep * steps_2__o = cc_step_none_new( CC_SLE_Start, 7, 7, CC_FSUE_Clarification_NoOutput );

            cc_step_none_append( steps_2__o, CC_SLE_Reposition, 9, 8, CC_FSUE_Clarification );

            CcSideEffect sse_2_1 = cc_side_effect_capture( CC_PE_LightBishop, CC_TE_None );
            cc_step_append( steps_2__o, CC_SLE_Distant, 7, 12, sse_2_1, CC_FSUE_User );

            CcSideEffect sse_2_2 = cc_side_effect_displacement( CC_PE_DarkKnight, false, 1, 5 );
            cc_step_append( steps_2__o, CC_SLE_Distant, 5, 1, sse_2_2, CC_FSUE_User );

            cc_step_none_append( steps_2__o, CC_SLE_Distant, 15, 6, CC_FSUE_Addition );

            CcStep * s = steps_2__o;
            while( s )
            {
                printf( "%p\n", (void *)s );
                s = s->next;
            }

            printf( TESTS_MOVE_NOTATION_SEPARATOR );


            CcStep * dup__o = cc_step_duplicate_all_new( steps_2__o );
            if ( !dup__o ) printf( "No dup!\n" );

            CcStep * d = dup__o;
            while( d )
            {
                printf( "%p\n", (void *)d );
                d = d->next;
            }

            cc_step_free_all_steps( &steps_2__o );

            cc_step_free_all_steps( &dup__o );

            printf( TESTS_MOVE_TEST_SEPARATOR );
        }
        else if ( !strcmp( "zz", cmd ) )
        {
            // int i = 3;
            // int j = 7;

            // printf( "Start: (%d, %d)\n", i, j );

            // for ( int k = 0; k < 10; ++k )
            // {
            //     if ( cc_gen_steps( &i, &j, 3, 2, true ) )
            //         printf( "Step %d: (%d, %d)\n", k, i, j );
            //     else
            //         printf( "Step %d fail!\n", k );
            // }

            int step_i = 7;
            int step_j = -2;

            printf( "Start: (%d, %d)\n", step_i, step_j );

            for ( int k = 0; k < 10; ++k )
            {
                cc_gen_steps( &step_i, &step_j, -1, 1, true );

                // if ( cc_gen_steps_is_valid( step_i, step_j, CC_GEN_STEPS_UNICORN, CC_GEN_STEPS_UNICORN_LEN ) )
                if ( CC_GEN_STEPS_UNICORN_IS_VALID( step_i, step_j ) )
                    printf( "Step %d: (%d, %d)\n", k, step_i, step_j );
                else
                    printf( "Step %d fail: (%d, %d)\n", k, step_i, step_j );
            }
        }
        else if ( !strcmp( "z2", cmd ) )
        {
            CcPosLink * pl = NULL;

            cc_pos_link_append_or_init( &pl, 1, 1 );
            cc_pos_link_append_or_init( &pl, 2, 2 );
            cc_pos_link_append_or_init( &pl, 3, 3 );
            cc_pos_link_append_or_init( &pl, 4, 4 );
            cc_pos_link_append_or_init( &pl, 5, 5 );
            cc_pos_link_append_or_init( &pl, 6, 6 );

            CcPosLink * x = pl;
            while ( x )
            {
                printf( "Pos: %d, %d (%p --> %p).\n", x->i, x->j, (void *)x, (void *)(x->next) );
                x = x->next;
            }
        }
        else if ( !strcmp( "z3", cmd ) )
        {
            CcPly * pl = NULL;

            cc_ply_append_or_init( &pl, CC_PLE_Ply, CC_PE_LightRook, NULL );
            cc_ply_append_or_init( &pl, CC_PLE_Teleportation, CC_PE_DarkUnicorn, NULL );
            cc_ply_append_or_init( &pl, CC_PLE_FailedTeleportation, CC_PE_LightUnicorn, NULL );
            cc_ply_append_or_init( &pl, CC_PLE_TranceJourney, CC_PE_DarkPawn, NULL );
            cc_ply_append_or_init( &pl, CC_PLE_DualTranceJourney, CC_PE_LightPawn, NULL );
            cc_ply_append_or_init( &pl, CC_PLE_PawnSacrifice, CC_PE_DarkRook, NULL );

            CcPly * x = pl;
            while ( x )
            {
                printf( "Ply: %d, %d (%p --> %p).\n", x->link, x->piece, (void *)x, (void *)(x->next) );
                x = x->next;
            }
        }
        else if ( !strcmp( "z4", cmd ) )
        {
            CcStep * st = NULL;
            CcSideEffect se = cc_side_effect_none();

            cc_step_append_or_init( &st, CC_SLE_Start, 1, 1, se, CC_FSUE_Debug );
            cc_step_append_or_init( &st, CC_SLE_Next, 2, 2, se, CC_FSUE_Debug );
            cc_step_append_or_init( &st, CC_SLE_Distant, 3, 3, se, CC_FSUE_Debug );
            cc_step_append_or_init( &st, CC_SLE_Next, 4, 4, se, CC_FSUE_Debug );
            cc_step_append_or_init( &st, CC_SLE_Distant, 5, 5, se, CC_FSUE_Debug );
            cc_step_append_or_init( &st, CC_SLE_Destination, 6, 6, se, CC_FSUE_Debug );

            CcStep * x = st;
            while ( x )
            {
                printf( "Step: %d: %d, %d (%p --> %p).\n", x->link, x->i, x->j, (void *)x, (void *)(x->next) );
                x = x->next;
            }
        }
        else
        {
            printf( "Unknown: '%s'.\n", buffer );
            // fflush( stdout );
        }

        free( cmd );
    }

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
