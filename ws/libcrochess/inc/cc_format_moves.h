// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PRINT_MOVES_H__
#define __CC_PRINT_MOVES_H__

// #include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_move.h"
#include "cc_ply.h"
#include "cc_step.h"

/**
    @file cc_format_moves.h
    @brief Enumerations, structures, and related functions to format move(s) as algebraic notation.
*/


/**
    Format move scope enumeration.
*/
typedef enum CcFormatMoveScopeEnum
{
    CC_FMSE_FormatOnlyCurrentMove,
    CC_FMSE_FormatOnlyLastMove,
    CC_FMSE_FormatAllMoves,
} CcFormatMoveScopeEnum;

/**
    Enumeration, when ply is to be wrapped into square brackets.
*/
typedef enum CcWrapPlyInSquareBracketsEnum
{
    CC_WPISB_Never, /**< Wrap never. */
    CC_WPISB_IfCascading_HasSteps, /**< Wrap, if move is cascading,
                                        and ply have step(s) before destination field
                                        e.g. a trance-journey with displacements. */
    CC_WPISB_IfCascading, /**< Wrap, if cascading move. */
    CC_WPISB_Always, /**< Wrap always. */
} CcWrapPlyInSquareBracketsEnum;

/**
    Format move structure.
*/
typedef struct CcFormatMove
{
    CcFormatMoveScopeEnum scope; /**< Scope, i.e. what move(s) to output.  */
    CcFormatStepUsageEnum usage; /**< Usage, i.e. which step(s) to output. */
    bool do_format_with_pawn_symbol; /**< Flag, if Pawn move(s) are formatted with its symbol,
                                          usually they are not.

                                          Does not affect output of Pawn symbol elsewhere, e.g.
                                          among captured pieces, in a dual trance-journey. */
    bool do_dark_pieces_uppercase; /**< Flag, whether dark piece symbols are uppercase (usual notation),
                                        or lowercase (for debugging).

                                        Does affect output of dark pieces symbols elsewhere, e.g.
                                        among captured pieces, in a dual trance-journey. */
    CcWrapPlyInSquareBracketsEnum wrap; /**< Wrap, when ply should be wrapped. */
    bool default_wrap; /**< Default wrapping, fall-back value. */
} CcFormatMove;

/**
    Returns move format structure.

    @param scope Scope, i.e. what move(s) to output.
    @param usage Usage, i.e. which step(s) to output.
    @param do_format_with_pawn_symbol Flag, if Pawn move(s) are formatted with its symbol,
                                      usually they are not.
    @param do_dark_pieces_uppercase Flag, whether dark piece symbols are uppercase (usual notation),
                                    or lowercase (for debugging).
    @param wrap Wrap, when ply should be wrapped.
    @param default_wrap Default wrapping, fall-back value.

    @return Move format structure.
*/
CcFormatMove cc_format_move( CcFormatMoveScopeEnum const scope,
                             CcFormatStepUsageEnum const usage,
                             bool const do_format_with_pawn_symbol,
                             bool const do_dark_pieces_uppercase,
                             CcWrapPlyInSquareBracketsEnum const wrap,
                             bool const default_wrap );

/** @defgroup format_move_convenience The format move conveniences
 *  The format move convenience functions are meant to be used instead of `cc_format_move()`.

    They have minimal set of arguments,
    otherwise behave exactly as their generic progenitor.

    @see cc_format_move()
 *  @{
 */

/**
    Returns move formatting flags, meant for output similar to user input,
    i.e. close to minimal algebraic notation.

    @param scope Scope, i.e. what move(s) to output.

    @return Move format structure.
*/
CcFormatMove cc_format_move_user( CcFormatMoveScopeEnum const scope );

/**
    Returns move formatting flags, meant for verbose short algebraic notation,
    e.g. all optional pieces and fields for side-effects are also present in output.

    @param scope Scope, i.e. what move(s) to output.

    @return Move format structure.
*/
CcFormatMove cc_format_move_output( CcFormatMoveScopeEnum const scope );

/**
    Returns move formatting flags, meant for debug short algebraic notation,
    e.g. dark pieces are lowercased.

    @param scope Scope, i.e. what move(s) to output.

    @return Move format structure.
*/
CcFormatMove cc_format_move_debug( CcFormatMoveScopeEnum const scope );

/** @} */ // end of step_convenience_new

/**
    Checks if a given ply, of a given move, should be wrapped in square brackets,
    based on formatting flags.

    @param move A move, owner of a `ply`.
    @param ply A ply, to format output.
    @param format_move Formatting flags.

    @return `true` if ply is to be wrapped, `false` otherwise.
*/
bool cc_if_wrap_ply_in_square_brackets( CcMove const * const restrict move,
                                        CcPly const * const restrict ply,
                                        CcFormatMove const format_move );

/**
    Returns lowercase character representing a file in algebraic notation.

    @param i A file, position along horizontal axis.

    @return A file character if successful, `?` otherwise.
*/
char cc_format_pos_file( int const i );

/**
    Returns a newly allocated string representing a rank in algebraic notation.

    @param j A rank, position along vertical axis.

    @return A newly allocated rank string if successful, `NULL` otherwise.
*/
char * cc_format_pos_rank_new( int const j );


// TODO :: DOCS
char * cc_format_lost_tag( CcTagEnum te );

/**
    Returns a newly allocated string containing formatted output of side-effect.

    @param side_effect A side-effect being formatted for algebraic notation output.
    @param format_move A format flags.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_side_effect_new( CcSideEffect const * const restrict side_effect,
                                  CcFormatMove const format_move );

/**
    Returns a newly allocated string containing formatted output of a step.

    @param move A move, owner of a `ply`.
    @param ply A ply, owner of a `step`.
    @param step A step being formatted for algebraic notation output.
    @param format_move A format flags.
    @param has_preceding_step_io Input/output flag, whether any previous step has been formatted into the output.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_step_new( CcMove const * const restrict move,
                           CcPly const * const restrict ply,
                           CcStep const * const restrict step,
                           CcFormatMove const format_move,
                           bool * const restrict has_preceding_step_io );

/**
    Returns a newly allocated string containing formatted output of a ply.

    @param move A move, owner of a `ply`.
    @param ply A ply being formatted for algebraic notation output.
    @param format_move A format flags.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_ply_new( CcMove const * const restrict move,
                          CcPly const * const restrict ply,
                          CcFormatMove const format_move );

/**
    Returns a newly allocated string containing formatted output of a move..

    @param move A move being formatted for algebraic notation output.
    @param format_move A format flags.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_move_new( CcMove const * const restrict move,
                           CcFormatMove const format_move );


#endif /* __CC_PRINT_MOVES_H__ */
