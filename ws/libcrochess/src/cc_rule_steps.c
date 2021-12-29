// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include <stddef.h>
// #include <math.h>

// #include "cc_defines.h"
// // #include "cc_pos.h"
// #include "cc_gen_steps.h"
// #include "cc_rule_steps.h"


// // TODO :: CONVERT :: use new iterator template
// bool cc_rule_steps_piece_pos_iter( CcChessboard * restrict cb,
//                                    char piece_symbol,
//                                    CcPieceEnum * restrict piece__o,
//                                    CcPos * restrict start__o,
//                                    bool initialize_iter )
// {
//     if ( !cb ) return false;
//     if ( !piece__o ) return false;
//     if ( !start__o ) return false;

//     if ( !cc_piece_is_symbol( piece_symbol ) ) return false;

// // TODO :: REMOVE :: STATIC
//     static int pos_i = 0;
//     static int pos_j = 0;
// // TODO :: REMOVE :: STATIC

//     if ( initialize_iter )
//     {
//         pos_i = 0;
//         pos_j = 0;
//     }

//     int size = (int)cb->size;

//     for ( int i = pos_i; i < size; ++i )
//     {
//         for ( int j = pos_j; j < size; ++j )
//         {
//             CcPieceEnum pe = cc_chessboard_get_piece( cb, i, j );

//             if ( cc_piece_symbol( pe ) == piece_symbol )
//             {
//                 // Next position to check.
//                 if ( j < size - 1 )
//                 {
//                     pos_j = j + 1;
//                     pos_i = i;
//                 }
//                 else
//                 {
//                     pos_j = 0;
//                     pos_i = i + 1;
//                 }

//                 *piece__o = pe;
//                 *start__o = cc_pos( i, j );
//                 return true;
//             }
//         }
//     }

//     return false;
// }
// // TODO :: CONVERT :: use new iterator template

// bool cc_rule_steps_find_piece_start_pos( CcChessboard * restrict cb,
//                                          CcPlyLinkEnum ple,
//                                          char piece_symbol,
//                                          int * restrict disamb_i__d,
//                                          int * restrict disamb_j__d,
//                                          int dest_i,
//                                          int dest_j,
//                                          CcPieceEnum * restrict piece__o,
//                                          CcPos * restrict start__o )
// {
//     if ( !cb ) return false;
//     if ( !piece__o ) return false;
//     if ( !start__o ) return false;

//     if ( !cc_piece_is_symbol( piece_symbol ) ) return false;

//     if ( disamb_i__d )
//         if ( !cc_chessboard_is_coord_on_board( cb, *disamb_i__d ) )
//             return false;

//     if ( disamb_j__d )
//         if ( !cc_chessboard_is_coord_on_board( cb, *disamb_j__d ) )
//             return false;

//     if ( !cc_chessboard_is_pos_on_board( cb, dest_i, dest_j ) )
//         return false;

//     CcPos dest = cc_pos( dest_i, dest_j );
//     CcPosLink * pos_s__a = NULL;

//     if ( disamb_i__d && disamb_j__d )
//     {
//         CcPieceEnum pe = cc_chessboard_get_piece( cb, *disamb_i__d, *disamb_j__d );

//         if ( cc_piece_symbol( pe ) == piece_symbol )
//         {
//             if (  cc_rule_steps_check_movement( cb, ple, pe,
//                                                 cc_pos( *disamb_i__d, *disamb_j__d ),
//                                                 dest,
//                                                 &pos_s__a ) )
//             {
//                 *piece__o = pe;
//                 *start__o = cc_pos( *disamb_i__d, *disamb_j__d );

//                 cc_pos_link_free_all( &pos_s__a );
//                 return true;
//             }
//         }

//         return false;
//     }

//     CcPieceEnum piece = CC_PE_None;
//     CcPos start = cc_pos_invalid();
//     bool init = true;

//     while ( cc_rule_steps_piece_pos_iter( cb, piece_symbol, &piece, &start, init ) )
//     {
//         init = false;

//         if ( ( disamb_i__d ) && ( *disamb_i__d != start.i ) ) continue;
//         if ( ( disamb_j__d ) && ( *disamb_j__d != start.j ) ) continue;

//         if ( cc_rule_steps_check_movement( cb, ple, piece, start, dest, &pos_s__a ) )
//         {
//             *piece__o = piece;
//             *start__o = start;

//             cc_pos_link_free_all( &pos_s__a );
//             return true;
//         }
//     }

//     return false;
// }


// bool cc_rule_steps_check_bishop( CcChessboard * restrict cb,
//                                  CcPlyLinkEnum ple,
//                                  CcPieceEnum piece,
//                                  CcPos start,
//                                  CcPos dest,
//                                  CcPosLink ** restrict pls__o )
// {
//     if ( !cb ) return false;
//     if ( !pls__o ) return false;
//     if ( *pls__o ) return false;

//     if ( !CC_PIECE_IS_BISHOP( piece ) ) return false;

//     CcPos offset = cc_pos_subtract( dest, start );
//     if ( ( offset.i == 0 ) || ( offset.j == 0 ) ) return false;
//     if ( abs( offset.i ) != abs( offset.j ) ) return false;

//     CcPos step = cc_pos( CC_SIGN( offset.i ), CC_SIGN( offset.j ) );
//     if ( !CC_GEN_POS_BISHOP_STEP_IS_VALID( step.i, step.j ) ) return false;

//     CcPosLink * pl__t = NULL;
//     for ( CcPos p = cc_pos_add( start, step ); !cc_pos_is_equal( p, dest ); p = cc_pos_add( p, step ) )
//     {
//         CcPieceEnum pe = cc_chessboard_get_piece( cb, p.i, p.j );
//         if ( !CC_PIECE_IS_NONE( pe ) ) return false;

//         if ( !cc_pos_link_append_pos_or_init( &pl__t, p ) )
//         {
//             cc_pos_link_free_all( &pl__t );
//             return false;
//         }
//     }

//     CcPieceEnum pe_dest = cc_chessboard_get_piece( cb, dest.i, dest.j );

//     if ( cc_piece_is_targetable( piece, pe_dest ) )
//     {
//         *pls__o = pl__t; // Ownership transfer --> pl__t is now weak pointer.
//         return true;
//     }

//     cc_pos_link_free_all( &pl__t );
//     return false;
// }

// bool cc_rule_steps_check_movement( CcChessboard * restrict cb,
//                                    CcPlyLinkEnum ple,
//                                    CcPieceEnum piece,
//                                    CcPos start,
//                                    CcPos dest,
//                                    CcPosLink ** restrict pls__o )
// {
//     // if ( !cb ) return false;
//     // if ( !pls__o ) return false;
//     // if ( *pls__o ) return false;

//     if ( CC_PIECE_IS_BISHOP( piece ) )
//         return cc_rule_steps_check_bishop( cb, ple, piece, start, dest, pls__o );
//     else
//         return false;
// }
