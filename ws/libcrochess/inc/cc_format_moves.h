// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PRINT_MOVES_H__
#define __CC_PRINT_MOVES_H__

// #include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_move.h"
#include "cc_ply.h"
#include "cc_step.h"

/**
    @file cc_format_moves.h
    @brief Format moves, plies, steps as algebraic notation strings.
*/


/**
    Defined constant for length of a zero-terminated rank string, i.e. `char` array.
*/
#define CC_FORMAT_RANK_LENGTH (3)

/**
    Defined constant for the size of a rank array.
*/
#define CC_FORMAT_RANK_SIZE (4)

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
    Rank format structure, used to pass short string by-value.
*/
typedef struct CcFormatRank
{
    char rank[ CC_FORMAT_RANK_SIZE ]; /**< Character array, i.e. zero-terminated rank string. */
} CcFormatRank;

/**
    Function returning all zero-ed rank array.

    @return Rank format with all zeroes in an rank array.
*/
CcFormatRank cc_format_rank_zero();

/**
    Function checking if rank format is all zero-ed.

    @param rank Rank format.

    @return `true` if rank array contains only zeros, `false` otherwise.
*/
bool cc_format_rank_is_zero( CcFormatRank rank );

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
CcFormatMove cc_format_move( CcFormatMoveScopeEnum scope,
                             CcFormatStepUsageEnum usage,
                             bool do_format_with_pawn_symbol,
                             bool do_dark_pieces_uppercase,
                             CcWrapPlyInSquareBracketsEnum wrap,
                             bool default_wrap );

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
CcFormatMove cc_format_move_user( CcFormatMoveScopeEnum scope );

/**
    Returns move formatting flags, meant for verbose short algebraic notation,
    e.g. all optional pieces and fields for side-effects are also present in output.

    @param scope Scope, i.e. what move(s) to output.

    @return Move format structure.
*/
CcFormatMove cc_format_move_output( CcFormatMoveScopeEnum scope );

/**
    Returns move formatting flags, meant for debug short algebraic notation,
    e.g. dark pieces are lowercased.

    @param scope Scope, i.e. what move(s) to output.

    @return Move format structure.
*/
CcFormatMove cc_format_move_debug( CcFormatMoveScopeEnum scope );

/** @} */ // end of step_convenience_new

/**
    Checks if a given ply, of a given move, should be wrapped in square brackets,
    based on formatting flags.

    @param move A move, owner of a `ply`.
    @param ply A ply, to format output.
    @param format_move Formatting flags.

    @return `true` if ply is to be wrapped, `false` otherwise.
*/
bool cc_if_wrap_ply_in_square_brackets( CcMove * restrict move,
                                        CcPly * restrict ply,
                                        CcFormatMove format_move );

/**
    Returns lowercase character representing a file in algebraic notation.

    @param i A file, position along horizontal axis.

    @return A file character if successful, `?` otherwise.
*/
char cc_format_pos_file( int i );

/**
    Returns a newly allocated string representing a rank in algebraic notation.

    @param j A rank, position along vertical axis.

    @return A newly allocated rank string if successful, `NULL` otherwise.
*/
char * cc_format_pos_rank__new( int j );

/**
    Returns formatted rank, with a string in a contained array.

    @param j A rank, position along vertical axis.

    @return Rank format with zero-terminated string in a rank array.
*/
CcFormatRank cc_format_pos_rank( int j );

/**
    Returns pointer to a constant string, based on a tag.

    @param te A tag.

    @return A pointer to non-empty string if tag can be lost,
            a pointer to empty string otherwise.
*/
char const * cc_format_lost_tag( CcTagEnum te );

/**
    Returns a newly allocated string containing formatted output of side-effect.

    @param side_effect A side-effect being formatted for algebraic notation output.
    @param format_move A format flags.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_side_effect__new( CcSideEffect * restrict side_effect,
                                  CcFormatMove format_move );

/**
    Returns a newly allocated string containing formatted output of a step.

    @param move A move, owner of a `ply`.
    @param ply A ply, owner of a `step`.
    @param step A step being formatted for algebraic notation output.
    @param format_move A format flags.
    @param has_preceding_step__io Input/output flag, whether any previous step has been formatted into the output.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_step__new( CcMove * restrict move,
                           CcPly * restrict ply,
                           CcStep * restrict step,
                           CcFormatMove format_move,
                           bool * restrict has_preceding_step__io );

/**
    Returns a newly allocated string containing formatted output of a ply.

    @param move A move, owner of a `ply`.
    @param ply A ply being formatted for algebraic notation output.
    @param format_move A format flags.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_ply__new( CcMove * restrict move,
                          CcPly * restrict ply,
                          CcFormatMove format_move );

/**
    Returns a newly allocated string containing formatted output of a move..

    @param move A move being formatted for algebraic notation output.
    @param format_move A format flags.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_format_move__new( CcMove * restrict move,
                           CcFormatMove format_move );


#endif /* __CC_PRINT_MOVES_H__ */
