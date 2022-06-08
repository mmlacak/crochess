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

    @note
    String pointer skips over almost all characters, and stops at ply link ones,
    if given `skip_or_stop_at` flag was `true`.

    @note
    If flag is `false`, string pointer will skip over ply link characters,
    and stop at any other.

    @see cc_ply_separator_len()

    @return String pointer if successful, `NULL` otherwise.
*/
char const * cc_traverse_ply_separator( char const * restrict an_str,
                                        bool skip_or_stop_at );



#endif /* __CC_PARSE_UTILS_H__ */
