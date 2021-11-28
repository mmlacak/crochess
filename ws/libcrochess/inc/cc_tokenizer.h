// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TOKENIZER_H__
#define __CC_TOKENIZER_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

/**
    @file cc_tokenizer.h
    @brief Tokenizer constants, and related functions.
*/


/**
    Tokenizer whitespace constants.
*/
extern char const CC_TOKEN_SEPARATORS_WHITESPACE[];

/**
    Tokenizer punctuation constants.
*/
extern char const CC_TOKEN_SEPARATORS_PUNCTUATION[];


/**
    Function returning if character is in the string.

    @param c Character to check.
    @param seps String of characters.

    @return `true` if character is found in string, `false` otherwise.
*/
bool cc_char_in( char const c, char const * const restrict seps );

/**
    Function traversing over string, returning next position within it.

    @param pos String to traverse.
    @param seps Separators to check.
    @param skip_or_stop_at Whether to skip separators (if `true`), or stop at them (if `false`).

    @return Next position within `pos` string if successful, `NULL` otherwise.
*/
char const * cc_traverse_chars( char const * const restrict pos,
                                char const * const restrict seps,
                                bool const skip_or_stop_at );

/**
    Function traversing over string, skipping separators, returning next position within string.

    @param pos String to traverse.
    @param seps Separators to skip.

    @return Next position within `pos` string if successful, `NULL` otherwise.
*/
char const * cc_skip_chars( char const * const restrict pos,
                            char const * const restrict seps );

/**
    Function traversing over string, stopping at separators, returning next position within string.

    @param pos String to traverse.
    @param seps Separators to stop at.

    @return Next position within `pos` string if successful, `NULL` otherwise.
*/
char const * cc_stop_at_chars( char const * const restrict pos,
                               char const * const restrict seps );


// // TODO :: DOCS
// /**
//     Iterator traversing over string, returning next token as newly allocated string.

//     @param str_s _Static_ string to traverse.
//     @param seps_s _Static_ separators to skip.

//     @note
//     Both `str_s` and `seps_s` are _static_ parameters, they have to be valid pointers for the first call, e.g.
//     ~~~{.c}
//     cc_next_token_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE );
//     ~~~

//     @note
//     Once iterator is initialized, both _static_ parameters can be `NULL`, e.g.
//     ~~~{.c}
//     cc_next_token_new( NULL, NULL );
//     ~~~

//     @note
//     Iterator will continue to return next token on each subsequent call, until it reaches end of an
//     original string (`str_s`), or is initialized again, with different strings.

//     @note
//     When reinitializing iterator, only parameter that has been changed needs to be passed, e.g.
//     ~~~{.c}
//     cc_next_token_new( NULL, CC_TOKEN_SEPARATORS_PUNCTUATION );
//     ~~~
//     will continue parsing next token in the original `str_s` string from the last position,
//     but with different set of separators.

//     @return Newly allocated string containing next token if successful, `NULL` otherwise.
// */
// // TODO :: DOCS
bool cc_next_token_iter_new( char const * const restrict str_s,
                             char const * const restrict seps,
                             char ** const restrict token_o,
                             bool const initialize_iter );

/**
    Function returning newly allocated string, with `chars` trimmed from the beginning and the end.

    @param str String to trim, on both ends.
    @param chars Characters to trim from string.

    @return Newly allocated, trimmed string if successful, `NULL` otherwise.
*/
char * cc_str_trim_new( char const * const restrict str,
                        char const * const restrict chars );

// size_t cc_flush_stdin();

#endif /* __CC_TOKENIZER_H__ */
