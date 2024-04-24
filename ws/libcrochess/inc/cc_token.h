// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TOKEN_H__
#define __CC_TOKEN_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

/**
    @file cc_token.h
    @brief Token constants, and related functions.
*/


/**
    Token whitespace constants.
*/
extern char const CC_TOKEN_SEPARATORS_WHITESPACE[];

/**
    Token punctuation constants.
*/
extern char const CC_TOKEN_SEPARATORS_PUNCTUATION[];


/**
    Function returning if character is in the string.

    @param c Character to check.
    @param seps String of characters.

    @return `true` if character is found in string, `false` otherwise.
*/
bool cc_char_in( char c, char const * seps );

/**
    Function traversing over string, returning next position within it.

    @param pos String to traverse.
    @param seps Separators to check.
    @param skip_or_stop_at Whether to skip separators (if `true`), or stop at them (if `false`).

    @return Next position within `pos` string if successful, `NULL` otherwise.
*/
char const * cc_traverse_chars( char const * pos,
                                char const * seps,
                                bool skip_or_stop_at );

/**
    Function traversing over string, skipping separators, returning next position within string.

    @param pos String to traverse.
    @param seps Separators to skip.

    @return Next position within `pos` string if successful, `NULL` otherwise.
*/
char const * cc_skip_chars( char const * pos,
                            char const * seps );

/**
    Function traversing over string, stopping at separators, returning next position within string.

    @param pos String to traverse.
    @param seps Separators to stop at.

    @return Next position within `pos` string if successful, `NULL` otherwise.
*/
char const * cc_stop_at_chars( char const * pos,
                               char const * seps );


/**
    Iterator traversing over string, returning next token as a pair
    of pointers.

    @param str String to traverse.
    @param seps A separators between tokens.
    @param start__io An _input_ / _output_ parameter.
    @param end__io An _input_ / _output_ parameter.

    @note
    Both _input_ / _output_ arguments `start__io` and `end__io` has to be valid
    pointers.
    Both inner pointers has to be `NULL` (i.e. `*start__io == NULL`, `*end__io == NULL`)
    at first call.
    At subsequent calls, both inner pointers has to be valid pointers.
    It is error if one inner pointer is valid, and the other `NULL`.

    @note
    Iterator will continue to return next token on each subsequent call, until
    end of a string is reached, or both inner pointers are initialized to `NULL`.

    @note
    Upon reaching end of a given string, both inner pointers (`*start__io` and
    `*end__io`) are reset to `NULL`. So, if nothing changes, next calls (or, next
    loop) will again start from the beginning of a given string `str`.

    @return `true` if next token was found, `false` otherwise.

    @return
    If `true` was returned, _input_ / _output_ argument `start__io` contains
    pointer to first `char` of a found token; argument `end__io` contains end
    of a token, i.e. first `char` that does not belong to a token.
*/
bool cc_iter_token( char const * str,
                    char const * seps,
                    char const ** start__io,
                    char const ** end__io );

/**
    Function returning newly allocated string, with `chars` trimmed from the beginning and the end.

    @param str String to trim, on both ends.
    @param chars Characters to trim from string.

    @return Newly allocated, trimmed string if successful, `NULL` otherwise.
*/
char * cc_trim_str__new( char const * str,
                         char const * chars );

// size_t cc_flush_stdin( void );

#endif /* __CC_TOKEN_H__ */
