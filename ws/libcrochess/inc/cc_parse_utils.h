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

    @param move_str A move, algebraic notation string.

    @return Length of a ply link if successful, `0` otherwise.

    @note For a first ply in a move, there is no preceeding ply, and so no link; returned value is also `0`.
*/
size_t cc_parse_utils_ply_link_len( char const * const restrict move_str );

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
    Function returns pointer to first step, within a given ply AN string.

    @param ply_str A ply, algebraic notation string.

    @return Valid pointer to first step if successful, `NULL` otherwise.
*/
char const * cc_parse_utils_get_steps_str( char const * const restrict ply_str );


#endif /* __CC_PARSE_UTILS_H__ */
