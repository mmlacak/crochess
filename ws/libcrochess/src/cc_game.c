// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_do_moves.h"
#include "cc_game.h"


CcGameStatusEnum cc_game_status_next( CcGameStatusEnum const gse,
                                      bool const is_resign,
                                      bool const is_end,
                                      bool const is_win )
{
    if ( is_resign )
    {
        if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Dark;
        if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Light;
    }

    if ( is_end )
    {
        if ( is_win )
        {
            if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Light;
            if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Dark;
        }
        else
            return CC_GSE_Draw;
    }

    if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Turn_Dark;
    if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Turn_Light;
    if ( gse == CC_GSE_None ) return CC_GSE_None;

    return gse;
}


CcGame * cc_game_new( CcGameStatusEnum status,
                      CcVariantEnum ve,
                      bool const do_setup )
{
    CcGame * gm = malloc( sizeof( CcGame ) );
    if ( !gm ) return NULL;

    gm->status = status;

    gm->chessboard = cc_chessboard_new( ve, do_setup );
    if ( !gm->chessboard )
    {
        free( gm );
        return NULL;
    }

    gm->moves = NULL;

    return gm;
}

bool cc_game_do_moves( CcGame * const restrict gm,
                       CcMove ** const restrict moves_n,
                       CcDoMoveEnum dme )
{
    if ( !gm ) return false;
    if ( !moves_n ) return false;
    if ( !*moves_n ) return false;

    if ( !cc_do_moves( gm->chessboard, *moves_n, dme ) )
        return false;

    if ( !cc_move_append_or_init( &( gm->moves ), moves_n ) )
        return false;

    return true;
}

bool cc_game_free_all( CcGame ** const restrict game_f )
{
    if ( !game_f ) return false;
    if ( !*game_f ) return true;

    bool result = true;

    CcChessboard ** cb = &( ( *game_f )->chessboard );
    result = cc_chessboard_free_all( cb ) && result;

    CcMove ** mv = &( ( *game_f )->moves );
    result = cc_move_free_all_moves( mv ) && result;

    free( *game_f );
    *game_f = NULL;

    return result;
}


/**
    Frees everything from all of given chessboard, moves, plies, steps.

    Function is meant as a convenience, to allow for one-liner to free
    allocated resources (even in the middle of building whole move data
    structure), before returning from a calling test function.

    @param gm_f A game, can be `NULL`.
    @param cb_f A chessboard, can be `NULL`.
    @param moves_f Linked list of moves, can be `NULL`.
    @param plies_f Linked list of plies, can be `NULL`.
    @param steps_f Linked list of steps, can be `NULL`.
    @param cumulative_result A cumulative result so far.

    @note
    Any of ponters can be `NULL`, in which case they are skipped,
    and don't affect result returned.

    Argument `cumulative_result` is meant for functions returning
    `bool` result, so tests could be writen as:
    @code{.c}
    bool result = true; // cumulative result of tests

    // Assuming test_1 returns bool too.
    // Putting it to first place is mandatory,
    // to ensure it's actually run, even if result is already `false`.
    result = test_1( ... ) && result;

    result = test_2( ... ) && result; // Ditto for test 2.

    return cc_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result ); // return cumulative result
    @endcode

    Failure can be enforced (e.g. when allocation fails),
    simply by passing `false` as an argument, e.g.:
    @code{.c}
    if ( !cc_ply_cascade_append_new( ... ) )
        return cc_move_data_free_all( NULL, &cb__o, NULL, &plies_0, &steps_2, false ); // false == enforced failure
    @endcode

    @return `true` if cumulatively successful, `false` otherwise.
*/
bool cc_move_data_free_all( CcGame ** const restrict gm_f,
                            CcChessboard ** const restrict cb_f,
                            CcMove ** const restrict moves_f,
                            CcPly ** const restrict plies_f,
                            CcStep ** const restrict steps_f,
                            bool const cumulative_result )
{
    bool results = true;

    if ( gm_f ) results = cc_game_free_all( gm_f ) && results;

    if ( cb_f ) results = cc_chessboard_free_all( cb_f ) && results;

    if ( moves_f ) results = cc_move_free_all_moves( moves_f ) && results;

    if ( plies_f ) results = cc_ply_free_all_plies( plies_f ) && results;

    if ( steps_f ) results = cc_step_free_all_steps( steps_f ) && results;

    return ( cumulative_result && results );
}
