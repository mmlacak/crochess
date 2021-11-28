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


/**
    Iterator traversing over string, returning next token as newly
    allocated string.

    @param str_s _Static_ string to traverse.
    @param seps A separators to skip.
    @param token_o An _output_ parameter.
    @param initialize_iter Flag, whether to initialize iterator.

    @note
    _Static_ `str_s` parameter has to be valid pointer even if iterator is not
    being intialized, to ensure string (buffer) is still allocated and readable.

    @note
    _Output_ parameter `token_o` has to have inner pointer initialized to `NULL`,
    i.e. `*token_o == NULL` has to hold true.

    @note
    If used within a loop, newly allocated string has to be `free()`-ed, or
    ownership transferred to some other variable, before each new loop.
    ~~~{.c}
    char * cmd = NULL;
    bool init = true;

    while ( !cc_next_token_iter_new( ..., ..., &cmd, init ) )
    {
        init = false;

        ... // some stuff

        free( cmd ); // Or transfer ownership.
        cmd = NULL; // Must be set to NULL.
    }
    ~~~

    @note
    Iterator will continue to return next token on each subsequent call,
    until it reaches end of an original string (`str_s`), or is initialized
    again with different string.

    @return `true` if next token was found, `NULL` otherwise.
    If `true` was returned, _output_ argument `token_o` contains a newly
    allocated copy of a token found.
*/
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
