// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stdlib.h>
#include <stdbool.h>

/**
    @file cc_parse_utils.h
    @brief Helper functions to parse algebraic notation.
*/


#define CC_PARSE_STR_LOST_TAG_LENGTH (2)

/**
    Function checks if a given character is a ply gathering.

    Ply gathering characters are `[` and `]`.

    @param c A character.

    @return `true` if ply gatherer, `false` otherwise.
*/
bool cc_is_char_ply_gatherer( char c );

/**
    Function returns length of a ply separator at the beginning of an algebraic notation string.

    @param an_str An algebraic notation string.

    @return Length of a ply separator if successful, `0` otherwise.
*/
size_t cc_ply_separator_len( char const * restrict an_str );

/**
    Function returns string pointer, starting from a given location, and moving toward end of string.

    @param an_str An algebraic notation string.
    @param skip_or_stop_at A flag, whether to skip ply link characters, or stop at first of them.
    @param exclude_ply_gatherers A flag, whether to exclude leading `[` and trailing `]` from ply notation.

    @note
    String pointer skips over almost all characters, and stops at ply link ones,
    if given `skip_or_stop_at` flag was `true`.

    @note
    If flag is `false`, string pointer will skip over ply link characters,
    and stop at any other.

    @see cc_ply_separator_len()

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_traverse_ply_separators( char const * restrict an_str,
                                         bool skip_or_stop_at,
                                         bool exclude_ply_gatherers );


/**
    Iterator traversing over string, returning next ply as a pair
    of pointers.

    @param move_an_str A complete move, algebraic notation string to traverse.
    @param first__io An _input_ / _output_ parameter.
    @param end__io An _input_ / _output_ parameter.

    @note
    Both _input_ / _output_ arguments `first__io` and `end__io` has to be valid
    pointers.
    Both inner pointers has to be `NULL` (i.e. `*first__io == NULL`, `*end__io == NULL`)
    at first call.
    At subsequent calls, both inner pointers has to be valid pointers.
    It is error if one inner pointer is valid, and the other `NULL`.

    @note
    Iterator will continue to return next ply on each subsequent call, until
    end of a string is reached, or both inner pointers are initialized to `NULL`.

    @note
    Upon reaching end of a given string, both inner pointers (`*first__io` and
    `*end__io`) are reset to `NULL`. So, if nothing changes, next calls (or, next
    loop) will again start from the beginning of an initially given string `an_str`.

    @return `true` if next ply was found, `false` otherwise.

    @return
    If `true` was returned, _input_ / _output_ argument `first__io` contains
    pointer to first `char` of a found ply; argument `end__io` contains end
    of a ply, i.e. first `char` that does not belong to a ply.
*/
bool cc_ply_iter( char const * restrict move_an_str,
                  char const ** restrict first__io,
                  char const ** restrict end__io );

/**
    Function getting piece symbol, for a given ply AN string.

    @param ply_an_str A ply, algebraic notation string.
    @param piece_symbol__o An _output_ parameter, returned piece symbol.

    @note
    Piece symbol is returned via _output_ parameter, not via return value.

    @note
    Piece symbol is a single character, as written in AN string; or `P` for a Pawn, if missing.

    @return `true` if successful, `false` otherwise.
*/
bool cc_get_ply_piece_symbol( char const * restrict ply_an_str,
                              char * restrict piece_symbol__o );




#endif /* __CC_PARSE_UTILS_H__ */
