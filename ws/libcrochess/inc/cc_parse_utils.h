// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>

#include "cc_ply.h"

/**
    @file cc_parse_utils.h
    @brief Helper functions to parse algebraic notation.
*/


/**
    Function checks if a given character is a ply gathering.

    Ply gathering characters are `[` and `]`.

    @param c A character.

    @return `true` if ply gatherer, `false` otherwise.
*/
bool cc_parse_utils_char_is_ply_gather( char const c );

/**
    Function returns string pointer, starting from a given location, and moving toward end of string.

    @param move_str A move, algebraic notation string.
    @param skip_or_stop_at A flag, whether to skip ply gather characters, or stop at first of them.

    String pointer skips over almost all characters, and stops at ply gather ones,
    if given `skip_or_stop_at` flag was `true`.

    If flag is `false`, string pointer will skip over ply gather characters,
    and stop at any other.

    @see cc_parse_utils_char_is_ply_gather

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_go_ply_gather( char const * const restrict move_str,
                                           bool const skip_or_stop_at );

/**
    Function returns length of a ply link in an algebraic notation, in characters.

    @param ply_str A ply, algebraic notation string.

    @return Length of a ply link if successful, `0` otherwise.
*/
size_t cc_parse_utils_ply_link_len( char const * const restrict ply_str );

/**
    Function returns string pointer, starting from a given location, and moving toward end of string.

    @param move_str A move, algebraic notation string.
    @param skip_or_stop_at A flag, whether to skip ply link characters, or stop at first of them.

    String pointer skips over almost all characters, and stops at ply link ones,
    if given `skip_or_stop_at` flag was `true`.

    If flag is `false`, string pointer will skip over ply link characters,
    and stop at any other.

    @see CcPlyLinkEnum
    @see cc_parse_utils_ply_link_len

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_go_ply_link( char const * const restrict move_str,
                                         bool const skip_or_stop_at );

/**
    Iterator returns newly allocated string, containing next ply algebraic notation.

    @param move_str_s A move, _static_ parameter, AN string used to initialize iterator, can be `NULL`.

    @note
    Iterator is initialized with valid `move_str_s` once, after which subsequent calls
    has to pass `NULL` as argument.

    @note
    Once AN string is traversed completely, `NULL` is returned.

    @return String pointer to newly allocated AN string if successful, `NULL` otherwise.
*/
char * cc_parse_utils_next_ply_str_new( char const * const restrict move_str_s );

/**
    Function getting ply link, for a given ply AN string.

    @param ply_str A ply, algebraic notation string.
    @param link_o An _output_ parameter, returned ply link.

    @note
    Ply link is returned via _output_ parameter, not via return value.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_ply_link( char const * const restrict ply_str,
                                  CcPlyLinkEnum * const restrict link_o );

/**
    Function getting piece, for a given ply AN string.

    @param ply_str A ply, algebraic notation string.
    @param is_light A flag, whether piece is light or dark (bright or dim, in case of Stars).
    @param piece_o An _output_ parameter, returned piece.

    @note
    Piece is returned via _output_ parameter, not via return value.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_ply_piece( char const * const restrict ply_str,
                                   bool const is_light,
                                   CcPieceEnum * const restrict piece_o );


/**
    Function returns pointer to first step, within a given ply AN string.

    @param ply_str A ply, algebraic notation string.

    @return Valid pointer to first step if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_get_steps_str( char const * const restrict ply_str );

/**
    Function returns length of a step link in an algebraic notation, in characters.

    @param step_str A step, algebraic notation string.

    @return Length of a step link if successful, `0` otherwise.
*/
size_t cc_parse_utils_step_link_len( char const * const restrict step_str );

/**
    Function returns string pointer, starting from a given location, and moving toward end of string.

    @param ply_str A ply, algebraic notation string.
    @param skip_or_stop_at A flag, whether to skip step link characters, or stop at first of them.

    String pointer skips over almost all characters, and stops at step link ones,
    if given `skip_or_stop_at` flag was `true`.

    If flag is `false`, string pointer will skip over step link characters,
    and stop at any other.

    @see CcStepLinkEnum
    @see cc_parse_utils_step_link_len

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_go_step_link( char const * const restrict ply_str,
                                          bool const skip_or_stop_at );

/**
    Iterator returns newly allocated string, containing next step algebraic notation.

    @param ply_str_s A ply, _static_ parameter, AN string used to initialize iterator, can be `NULL`.

    @note
    Iterator is initialized with valid `ply_str_s` once, after which subsequent calls
    has to pass `NULL` as argument.

    @note
    Once AN string is traversed completely, `NULL` is returned.

    @return String pointer to newly allocated AN string if successful, `NULL` otherwise.
*/
char * cc_parse_utils_next_step_str_new( char const * const restrict ply_str_s );

/**
    Function returns whether given ply has multiple steps.

    @param ply_str A ply, algebraic notation string.

    @return `true` if ply has multiple steps, `false` otherwise.
*/
bool cc_parse_utils_ply_has_multiple_steps( char const * const restrict ply_str );

/**
    Function getting step link, for a given step AN string.

    @param ply_str A ply, algebraic notation string.
    @param step_str A step, algebraic notation string.
    @param link_o An _output_ parameter, returned step link.

    @note
    Step link is returned via _output_ parameter, not via return value.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parse_utils_get_step_link( char const * const restrict ply_str,
                                   char const * const restrict step_str,
                                   CcStepLinkEnum * const restrict link_o );

/**
    Function returns pointer to first character of a side effect, in a given step.

    @param step_str A step, algebraic notation string.

    @note
    Returned pointer points to first character of a side effect, if it exists.

    @note
    If it doesn't, returned pointer points to end of string, i.e. `\0` character.

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_stop_at_side_effects( char const * const restrict step_str );

/**
    Function returns newly allocated string containing fields string notation.

    @param step_str A step, algebraic notation string.

    @return Newly allocated string pointer if successful, `NULL` otherwise.
*/
char * cc_parse_utils_step_fields_str_new( char const * const restrict step_str );

/**
    Function returns string pointer to side-effect, within given step AN string.

    @param step_str A step, algebraic notation string.

    @see cc_parse_utils_stop_at_side_effects()

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_side_effect_str( char const * const restrict step_str );

// *** TODO :: DOCS ***
bool cc_parse_utils_get_fields( char const * const restrict fields_str,
                                int * restrict disambiguation_file_o,
                                int * restrict disambiguation_rank_o,
                                int * restrict file_o,
                                int * restrict rank_o );

#endif /* __CC_PARSE_UTILS_H__ */
